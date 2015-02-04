;;; vimizer.el --- Make Emacs's cut/copy/paste more like Vim's -*- lexical-binding: t; -*-
;; Version: 0.2.6
;; Package-Requires: ((emacs "24.4"))
;; Keywords: convenience

;; This file doesn't use hard word wrap. To fold away the long comments and docstrings, use:
;; (setq truncate-lines t)
;; or with Usablizer's functions, use:
;; (set-line-wrap 'off) ; or press M-S-right to cycle to that setting
;; To show the long lines, use:
;; (set-line-wrap 'word) ; or press M-S-right


;;; Commentary:
;;
;; Features:
;;
;; 0. Vim-style cut and copy transient modes.
;; Press Cut or Copy to enter cut or copy transient mode (if the region isn't active); then the next command, if it's a motion command, will result in the text moved over by that command being cut or copied. You can provide a prefix arg before or after entering the transient mode, which will cut or copy the text moved over by repeating the motion command the specified number of times, like in Vim.
;; The transient mode is always automatically turned off after the next command (regardless of whether that command is a motion command), which is why the mode called a ‟transient” mode. Thus, if you press Cut C-f C-f, then only one character will be cut, and the next character will just be moved over as usual.
;; If the region is active, then Cut or Copy will simply cut or copy the region instead of entering the transient mode.
;;
;; 1. Vim-style line-based cutting, copying, pasting, and selection, and repeatable pasting.
;; Press these keys for these features:
;; S-Cut or S-Copy: cut or copy the current logical line, regardless of where the cursor currently is on the line. Use a prefix arg to cut or copy the specified number of lines.
;; Paste: paste text from the head of the clip ring. Use a prefix arg to paste the specified number of copies of that text.
;; S-Paste: paste text from the head of the clip ring to a new line over the current logical line, regardless of where the cursor currently is on the line, and regardless of whether the pasted text was originally cut or copied as a full line. Because of this, if you press S-Cut followed by S-Paste, the text is cut and then pasted back in the same place, effectively undoing the cut. If you press S-Copy followed by S-Paste, the current logical line is duplicated.
;; S-SunFront: enter line-select mode. In this mode, complete logical lines are selected and highlighted, regardless of which commands you use to move the cursor. You can then cut or copy the selected text or do anything else that uses an active region. To cancel the mode, press whatever key or chord you have bound to keyboard-quit (C-g by default in Emacs).
;; M-Cut, M-Copy, M-S-Cut, or M-S-Copy: do the same as without M, but append the cut or copied text to the end of the text at the head of the clip ring rather than pushing the cut or copied text to a new element on the clip ring.
;; M-Paste: reverse rotate through the clip ring and replace the last pasted text (this is Emacs's standard yank-pop). Press M-S-Paste to forward rotate. Press s-Paste to paste the primary selection (OS-dependent).
;; SunFront: enter standard text-select mode (this is Emacs's standard push-mark-command).
;; M-SunFront: enter rectangle-select mode (Emacs's standard rectangle-mark-mode).
;;
;; If you don't have Cut, Copy, Paste, and SunFront keys, or if they're not in convenient locations, then get a better keyboard.
;; By the time the Cut, Copy, and Paste keys' names make their way to Emacs on X on Debian Linux (other systems not tried), those names are prefixed with ⌜XF86⌝, so the keybindings in this package use those prefixed names.
;;
;; Additional Vim-inspired features for Emacs are in Usablizer, a companion package for Vimizer.
;;
;; Unlike other vimization packages for Emacs (and Vim itself), Vimizer and Usablizer don't separate insertion and command modes, because such separation results in accidentally inserting the names of commands into buffers when you try to execute the commands, and accidentally mangling buffers with random edits when you try to insert text. Instead, these packages provide modeless bindings to function keys, so the commands are always available.


;;; Code:

(eval-when-compile (require 'cl))

(eval-and-compile ; Silence the byte compiler warning
  (defvaralias 'clip-ring 'kill-ring)) ; The ring of clippings includes not only ‟killed” (i.e. cut) things, but also copied things, like a clipboard does
(defalias 'paste-rotate-reverse 'yank-pop) ; ‟pop” implies consumption, which yank-pop doesn't actually do
(defvar dynamic-cursor-mode nil) ; Implemented in Nicizer


;;; Utilities

;; My simple patches for beginning-of-buffer, end-of-buffer, and yank to take nomsg option, yank to take nopushmark option, and set-mark to take dont-activate option, are cleaner, but they aren't wanted, so this kludge is necessary.
(defmacro define-function-suppressor (wrapper victim docstring)
  "Make a macro named WRAPPER (a symbol), with DOCSTRING, that takes a body and evaluates it with function VICTIM suppressed."
  `(defmacro ,wrapper (&rest body) ,docstring
	     `(cl-letf (((symbol-function ',',victim) (lambda (&rest _dummy) ())))
		,@body)))

(define-function-suppressor silently message "Do BODY without messaging anything.")
(define-function-suppressor not-pushily push-mark "Do BODY without pushing the mark.")
(define-function-suppressor not-activatingly activate-mark "Do BODY without activating the mark.")

(defmacro define-blmm-suppression-conspiracy (victim &rest body)
  "Define a list of agents that buffer-locally suppress minor mode VICTIM.
Also define a pair of functions to add/remove suppressors on the list.
When the last suppressor is removed, VICTIM is restored to its previous state,
and optional BODY forms are run. BODY must return a non-nil value."
  (let* ((victim-name (symbol-name victim))
	 (blmms-sym (intern (concat victim-name "-suppressors"))))
    `(progn
       (defvar-local ,blmms-sym nil
	 ,(concat "List of agents that are currently suppressing `" victim-name "'."))

       (defun ,(intern (concat "unsuppress-" victim-name)) (suppressor)
	 ,(concat "Remove SUPPRESSOR from `" victim-name "-suppressors'.
If no suppressors remain, restore `" victim-name "' to its previous state,
and return a non-nil value. If any suppressors remain, return nil.")
	 (let ((blmms (setq ,blmms-sym (delq suppressor ,blmms-sym))))
	   (let ((x (or (null blmms)
			(and (null (cdr blmms)) ; There's only one element
			     (assq 'blmm-orig-val blmms))))) ; And it's just that cons
	     (when x ; No suppressors remain
	       (if (consp x) ; But there is blmm-orig-val
		   (progn
		     (setq-local ,victim (cdr x))
		     (setq ,blmms-sym nil))
		 (assert (null blmms))
		 (kill-local-variable ',victim))
	       t ; In case no body follows
	       ,@body))))

       (defun ,(intern (concat "suppress-" victim-name)) (suppressor)
	 ,(concat "Add SUPPRESSOR to `" victim-name "-suppressors'.
Buffer-locally turn off `" victim-name "'.")
	 (unless ,blmms-sym
	   (if (assq ',victim (buffer-local-variables))
	       (setq ,blmms-sym
		     (list (cons 'blmm-orig-val ,victim)))))
	 (setq-local ,victim nil)
	 (pushnew suppressor ,blmms-sym)))))

(define-blmm-suppression-conspiracy shift-select-mode)
(define-blmm-suppression-conspiracy show-paren-mode)
(define-blmm-suppression-conspiracy dynamic-cursor-mode
  (if dynamic-cursor-mode (setq cursor-type (if mark-active 'bar t)) t))
(define-blmm-suppression-conspiracy cursor-type)

(defun clip-ring-maybe-append-eol ()
  "If no EOL char at end of element at head of clip ring, append one."
  (let* ((ccr (car clip-ring))
	 (lccr (length ccr)))
    (if (and (> lccr 0)
	     (not (equal ?\n (aref ccr (1- lccr)))))
	(kill-append "\n" nil))))

(defun not-presumptuous-cut-region (start end &optional msg append region)
  "Cut the region.
Prevent `kill-region' from presumptuously appending to previously cut text if `cctm-maybe-cut-copy', `modal-cut', `cutline', or anything else is called twice in a row, or after anything else which did kill-region. If optional MSG is non-nil, print a message if region is empty.
REGION is passed as the new argument to kill-region as of Emacs 24.4, the purpose of which is to confuse you."
  (let ((kill-read-only-ok t) ; Otherwise I'd have to handle the signal from kill-region to avoid screwups
	(empty (eq start end)))
    (setq last-command (if append 'kill-region 'bugger-off))
    (if append (clip-ring-maybe-append-eol))
    (kill-region start end region)
    (if (and empty msg) (message "Cut empty region")))
  (setq this-command 'bugger-off))

(defun not-presumptuous-copy-region (start end &optional msg append region)
  "Analogous to `not-presumptuous-cut-region`."
  (let ((empty (eq start end)))
    (setq last-command (if append 'kill-region 'bugger-off))
    (if append (clip-ring-maybe-append-eol))
    (kill-ring-save start end region)
    (if (and empty msg) (message "Copied empty region")))
  (setq this-command 'bugger-off))

(defun cut-copy-line (cut-copy-func is-cut arg append)
  "Cut or copy the current logical line.
If region is active, then operate on all lines which are at least partially included in region."
  (cctm-exit) ; Avoid screwing up if cutline is called while in cut or copy transient mode
  (save-excursion
    (if (and mark-active (> (point) (mark)))
	(exchange-point-and-mark))
    (move-beginning-of-line nil)
    (let ((start (point)))
      (if mark-active
	  (progn
	    (exchange-point-and-mark)
	    (unless (bolp) (forward-line))) ; Extend active region to fully include the partially-included last line
	(forward-line arg)) ; If region not active
      (let ((landing-bolp (bolp))
	    (landing-point (point)))
	(funcall cut-copy-func start (point) nil append)
	(if (= start landing-point)
	    (message "%s empty region" (if is-cut "Cut" "Copied"))) ; Not necessarily at end of buffer; arg could have been 0
	(if (not landing-bolp)
	    (kill-append "\n" nil)))))) ; In case forward-line reached end of buffer and there was no EOL at the end. (I.e. end-of-buffer is serving as implicit EOL.)

(defun setnil (arg) (set arg nil))

(defun global-set-key-list (list)
  "Map `global-set-key' to LIST of keybindings."
  (mapc (lambda (x) (global-set-key (car x) (cadr x)))
	list))


;;; Modeless editing commands

;; XXX: Elisp docstring syntax has no way to include other strings, so I can factor out common text?
(defun cutline (&optional arg append)
  "Cut the current logical line, or ARG lines.
If region is active, then operate on all lines which are at least partially included in region."
  (interactive "p")
  (cut-copy-line #'not-presumptuous-cut-region t (or arg 1) append))

(defun cutline-append (arg)
  "Do `cutline', but append the cut text to the element at the head of the clip ring instead of pushing a new element."
  (interactive "p")
  (cutline arg t))

(defun copyline (&optional arg append)
  "Copy the current logical line, or ARG lines.
If region is active, then operate on all lines which are at least partially included in region."
  (interactive "p")
  (cut-copy-line #'not-presumptuous-copy-region nil (or arg 1) append))

(defun copyline-append (arg)
  "Do `copyline', but append the copied text to the element at the head of the clip ring instead of pushing a new element."
  (interactive "p")
  (copyline arg t))

(defun not-weird-paste (&optional arg)
  "Paste the head of the clip ring. Optionally repeat ARG times.
This function replaces the weird and unnecessary arg handling of Emacs's standard `yank' command, which reverse rotates the clip ring (ARG - 1) times before pasting."
  (interactive "p")
  (unless arg (setq arg 1))
  (when (and (> arg 0) mark-active)
    (if delete-active-region
	(delete-region (mark) (point))) ; Emacs replaces active region if I press C-y to call yank directly, but if I call yank here in this function, it doesn't.
    (line-select-minor-mode-disable)) ; Emacs apparently doesn't call deactivate-mark-hook (triggered by the above call to delete-region), and thus deactivate line-select minor mode, until after post-command-hook, so lsmm-dominate-point-mark would be superfluously called (and incorrectly position the point since it calculated the target position before the region was deleted), so I have to disable it here to prevent that.
  (while (> arg 0)
    (silently (yank))
    (setq arg (1- arg))))

(defun paste-over (&optional arg)
  "Paste over current logical line. Optionally repeat ARG times."
  (interactive "p")
  (unless arg (setq arg 1))
  (when (> arg 0)
    (when mark-active
      (if delete-active-region
	  (delete-region (mark) (point))) ; See comment in not-weird-paste about this
      (line-select-minor-mode-disable)) ; And about this too
    (move-beginning-of-line nil)
    (push-mark (point) t nil)
    (while (> arg 0)
      (not-pushily (yank))
      (unless (bolp) (newline))
      (setq arg (1- arg)))))

;; I don't need paste-under after all (I was brainwashed by Vim); paste-over suffices
(defun paste-under (&optional arg)
  "Paste under current logical line. Optionally repeat ARG times."
  (interactive "p")
  (unless arg (setq arg 1))
  (when (> arg 0)
    (when mark-active
      (if delete-active-region
	  (delete-region (mark) (point))) ; See comment in not-weird-paste about this
      (line-select-minor-mode-disable)) ; And about this too
    (let ((start (point)))
      (forward-line)
      (unless (bolp) (newline)) ; In case forward-line reached end of buffer and there was no EOL at the end
      (if (= start (point))
	  (message "Illogical to paste after end of buffer. Use standard paste instead.")
	(progn
	  (push-mark (point) t nil)
	  (while (> arg 0)
	    (not-pushily (yank))
	    (unless (bolp) (newline)) ; In case last line of pasted text had no trailing EOL
	    (setq arg (1- arg))))))))

(defun paste-rotate (&optional arg)
  "Like `paste-rotate-reverse', but with forward rotation of the clip ring."
  (interactive "p")
  (yank-pop (if arg (- arg) -1)))

(defun paste-primary ()
  "Insert at point the primary selection.
This is the equivalent of `mouse-yank-primary', but suitable for keyboard binding."
  (interactive)
  (let ((mouse-yank-at-point t)) (mouse-yank-primary nil)))


;;; Vim-style cut-copy transient mode

(defconst cctm-activators
  '(modal-cut modal-copy modal-cut-append modal-copy-append)
  "List of commands that activate cut-copy transient mode.")

(defconst cctm-aborters
  '(self-insert-command backward-delete-char-untabify backward-delete-word delete-forward-char yank not-weird-paste paste-over paste-under undo undo-tree-undo undo-tree-redo eval-region-or-last-sexp)
  "List of commands that can move point, but don't trigger cut or copy when in cut-copy transient mode.")

(defconst cctm-vars
  '(cctm-cut-mode
    cctm-copy-mode
    cctm-append
    cctm-anchor
    cctm-buffer-of-anchor
    cctm-window-of-anchor
    cctm-frame-of-anchor
    cctm-original-blink-cursor-status))

;; Doesn't work, since defvar is not a function:
;; (mapc #'defvar cctm-vars)

;; Doesn't work either:
;; (dolist (i cctm-vars)
;;   (defvar i nil))

;; So I have to do it manually. Yay, elisp.
;; These are all intentionally not buffer-local
(defvar cctm-cut-mode nil)
(defvar cctm-copy-mode nil)
(defvar cctm-append nil)
(defvar cctm-anchor nil)
(defvar cctm-buffer-of-anchor nil)
(defvar cctm-window-of-anchor nil)
(defvar cctm-frame-of-anchor nil)
(defvar cctm-original-blink-cursor-status nil)

(defun modal-cut (&optional arg append)
  "Cut using Vim-style motion-composed transient mode.
Do standard cut if region is active; otherwise, cut the range moved over by the next command.
Optional ARG is passed to the next command."
  (interactive "P")
  (if mark-active
      (not-presumptuous-cut-region (region-beginning) (region-end) t append t)
    (unless (memq last-command cctm-activators) (push-mark (point) t)) ; Just for the user's convenience
    (setq cctm-cut-mode t)
    (setq cctm-copy-mode nil)
    (if dynamic-cursor-mode (setq cursor-type 'bar))
    (cctm-enter-common arg append)))

(defun modal-copy (&optional arg append)
  "Copy using Vim-style motion-composed transient mode.
Do standard copy if region is active; otherwise, copy the range moved over by the next command.
Optional ARG is passed to the next command."
  (interactive "P")
  (if mark-active
      (not-presumptuous-copy-region (region-beginning) (region-end) t append t)
    (unless (memq last-command cctm-activators) (push-mark (point) t)) ; Just for the user's convenience
    (setq cctm-cut-mode nil)
    (setq cctm-copy-mode t)
    (if dynamic-cursor-mode (setq cursor-type t))
    (cctm-enter-common arg append)))

(defun modal-cut-append (arg)
  "Do `modal-cut', but append the cut text to the element at the head of the clip ring instead of pushing a new element."
  (interactive "P")
  (modal-cut arg t))

(defun modal-copy-append (arg)
  "Do `modal-copy', but append the copied text to the element at the head of the clip ring instead of pushing a new element."
  (interactive "P")
  (modal-copy arg t))

;; cctm entry actions for both cut and copy
(defun cctm-enter-common (&optional arg append)
  (if arg (setq prefix-arg arg)) ; Pass through prefix argument to the next command
  (setq cctm-append append)
  (setq cctm-anchor (point)
	cctm-buffer-of-anchor (current-buffer)
	cctm-window-of-anchor (selected-window)
	cctm-frame-of-anchor (selected-frame))
  (unless cctm-original-blink-cursor-status ; In case user activates twice
    (setq cctm-original-blink-cursor-status (if blink-cursor-mode 1 0)))
  (blink-cursor-mode) ; Visually indicate cctm is active
  (add-hook 'post-command-hook 'cctm-maybe-cut-copy)) ; Intentionally not buffer-local

;; If in cut-copy transient mode, do cut or copy with next motion command
(defun cctm-maybe-cut-copy ()
  (assert (or cctm-cut-mode cctm-copy-mode))
  (unless (or (memq this-command cctm-activators) ; Motion is next command, not this one
	      prefix-arg) ; This command was uarg, so wait for next non-uarg command before acting, in order to allow the uarg to apply to the motion command
    (unwind-protect
	(unless (or (= cctm-anchor (point)) ; If command was not motion, don't do cut or copy
		    (memq this-command cctm-aborters)
		    (> cctm-anchor (point-max))) ; Don't screw up if something unexpectedly changed
	  ;; Don't screw up if different buffer/window/frame selected
	  (if (and (eq (current-buffer) cctm-buffer-of-anchor)
		   (eq (selected-window) cctm-window-of-anchor)
		   (eq (selected-frame) cctm-frame-of-anchor))
	      (let ((start (min cctm-anchor (point)))
		    (end (max cctm-anchor (point)))) ; Direction matters to prevent kill-append from prepending instead of appending
		(if cctm-cut-mode
		    (not-presumptuous-cut-region start end nil cctm-append)
		  ;; cctm-copy-mode here assured by the «assert» above
		  (not-presumptuous-copy-region start end nil cctm-append)))))
      (cctm-exit)))) ; Yes, abort even when just moving focus to another window. Leaving cut or copy transient mode enabled would just lead to user accidents.

(defun cctm-exit ()
  (save-selected-window ; In case top-level command selected a different window
    (if cctm-window-of-anchor (select-window cctm-window-of-anchor t))
    (if (and dynamic-cursor-mode (not mark-active)) ; Last command might have set mark active, and activating mark sets cursor type to bar in dynamic-cursor-mode, so don't interfere with that.
	(setq cursor-type t)))
  (blink-cursor-mode
   (case cctm-original-blink-cursor-status
     (0 0)
     (1 1)
     ;; Needed for when cctm-exit is called when cctm isn't active
     (t (or blink-cursor-mode 0)))) ; I.e. don't change it
  (mapc #'setnil cctm-vars)
  (remove-hook 'post-command-hook 'cctm-maybe-cut-copy))


;;; Vim-style line-select minor mode
;; The implementation is necessarily hairy in order to work exactly right. In particular:
;; crossing the anchor line without glitches
;; allowing arbitrary motion commands
;; correctly operating at the end of the buffer regardless of whether there's a newline at the end
;; sensibly handling exchange-point-and-mark
;; sensibly handling mode activation when there's already an active region spanning multiple logical lines

(defvar-local lsmm-anchor-line-beginning nil)
(defvar-local lsmm-anchor-line-end nil)
(defvar-local lsmm-point-is-past-anchor nil)
(defvar-local lsmm-active nil) ; When the function line-select-minor-mode runs, the variable of the same name only says whether mode is currently active, not whether mode was already active before the function was called, so lsmm-active is necessary as a separate variable.

(defvar line-select-minor-mode-map (make-keymap))
(mapc (lambda (x) (define-key line-select-minor-mode-map (car x) (cadr x)))
      '(([remap previous-line] backward-line)
	([remap previous-logical-line] backward-line)
	([remap next-line] forward-line)
	([remap next-logical-line] forward-line)
	([remap exchange-point-and-mark] lsmm-exchange-point-mark)))

(define-minor-mode line-select-minor-mode
  "Select logical lines.
Select the current logical line, and select more logical lines when point is moved. If the mark is already active, expand the current region to include whole logical lines."
  nil " Line-Select" 'line-select-minor-mode-map
  (if (not line-select-minor-mode)
      (when lsmm-active
	(remove-hook 'post-command-hook 'lsmm-dominate-point-mark t)
	(remove-hook 'deactivate-mark-hook 'line-select-minor-mode-disable t)
	(remove-hook 'rotate-mark-ring-hook 'lsmm-disable-and-deactivate-mark t)
	(unsuppress-shift-select-mode 'line-select-minor-mode)
	(unsuppress-show-paren-mode 'line-select-minor-mode)
	(unsuppress-cursor-type 'line-select-minor-mode)
	(unsuppress-dynamic-cursor-mode 'line-select-minor-mode)
	(setq lsmm-active nil))
    (unless transient-mark-mode
      (user-error "line-select not compatible with your luddite config"))
    (unless lsmm-active ; Line-select mode might already be active, so don't re-init
      (suppress-shift-select-mode 'line-select-minor-mode)
      (suppress-show-paren-mode 'line-select-minor-mode)
      (add-hook 'post-command-hook 'lsmm-dominate-point-mark nil t)
      (add-hook 'deactivate-mark-hook 'line-select-minor-mode-disable t)
      (add-hook 'rotate-mark-ring-hook 'lsmm-disable-and-deactivate-mark nil t)
      (setq lsmm-active t))
      ;; TODO: draw a thin horizontal black line the full width of the window, below the last selected text line (or above it, if point is before mark), for the same reason that I switch to vertical black line cursor between characters when mark is active and line-select mode isn't active. Then visually, the cursor has become that horizontal line.
    (lsmm-set-anchors nil)))

;;Avoid the problem described in my May 28, 2013 msg to help-gnu-emacs@gnu.org
(defun lsmm-disable-and-deactivate-mark ()
  (line-select-minor-mode-disable)
  (deactivate-mark))

(defun line-select-minor-mode-disable ()
  (line-select-minor-mode 0))

(defun line-select-minor-mode-enable ()
  "Enable `line-select-minor-mode' without superfluous message'." ; Mode is already indicated on the mode line
  (interactive)
  (line-select-minor-mode))

(defun backward-line (&optional arg)
  "Opposite of Emacs's built-in `'forward-line', but with the latter's negation fixed."
  (interactive "p")
  (unless arg (setq arg 1))
  (if (< arg 1) (forward-line (- arg))
    (when (not (bolp))
      (move-beginning-of-line nil)
      (setq arg (1- arg)))
    (forward-line (- arg))))

;; Set the start and end anchors for line-select mode.
;; If the mark is active and RESET is nil, extend the current region to include whole logical lines rather than resetting the region to include just the current logical line.
(defun lsmm-set-anchors (reset)
  (suppress-dynamic-cursor-mode 'line-select-minor-mode)
  (suppress-cursor-type 'line-select-minor-mode)
  (if (and mark-active (not reset))
      (let* ((old-mark (mark))
	     (old-point (point))
	     (forward (>= old-point old-mark)))
	(goto-char old-mark)
	(if (or forward (not (bolp))) (forward-line))
	(not-activatingly (set-mark (point)))
	(setq lsmm-anchor-line-end (point))
	(backward-line)
	(setq lsmm-anchor-line-beginning (point))
	(goto-char old-point) ; post-command-hook has lsmm-dominate-point-mark which will adjust point as necessary after this
	(setq lsmm-point-is-past-anchor (> (point) lsmm-anchor-line-beginning)))
    (forward-line)
    (push-mark (point) t t)
    (setq lsmm-anchor-line-end (point))
    (backward-line)
    (setq lsmm-anchor-line-beginning (point))
    (setq lsmm-point-is-past-anchor nil)))

;; Ensure selection of whole lines when in line-select mode
(defun lsmm-dominate-point-mark ()
  (assert line-select-minor-mode)
  (if (<= (point) lsmm-anchor-line-beginning)
      (progn
	(not-activatingly (set-mark lsmm-anchor-line-end))
	(move-beginning-of-line nil)
	(if (and lsmm-point-is-past-anchor (eq this-command 'backward-line))
	    (backward-line))
	(setq lsmm-point-is-past-anchor nil))
    (not-activatingly (set-mark lsmm-anchor-line-beginning))
    (unless (bolp) (forward-line))
    (if (and (not lsmm-point-is-past-anchor) (eq this-command 'forward-line))
	(forward-line))
    (setq lsmm-point-is-past-anchor t)))

(defun lsmm-exchange-point-mark ()
  "Like `exchange-point-and-mark', but specialized for `line-select-minor-mode'."
  (interactive)
  (let ((oalb lsmm-anchor-line-beginning)
	(oale lsmm-anchor-line-end)
	(opipa lsmm-point-is-past-anchor))
    (if opipa (progn
		(backward-line)
		(lsmm-set-anchors t)
		(goto-char oalb))
      (lsmm-set-anchors t)
      (goto-char oale)
      (not-activatingly (set-mark lsmm-anchor-line-beginning))
      (setq lsmm-point-is-past-anchor t))))


;;; Init

;;;###autoload
(defun vimizer-bind-keys ()
  " Bind the Cut, Copy, Paste, and SunFront keys to Vimizer's functions.
See the Commentary section of vimizer.el for how these work."
  (interactive)
  (global-set-key-list
   '(

     ;;Editing commands
     ([XF86Cut] modal-cut)
     ([XF86Copy] modal-copy)
     ([S-XF86Cut] cutline)
     ([S-XF86Copy] copyline)
     ([M-XF86Cut] modal-cut-append)
     ([M-XF86Copy] modal-copy-append)
     ([M-S-XF86Cut] cutline-append)
     ([M-S-XF86Copy] copyline-append)
     ([XF86Paste] not-weird-paste)
     ([S-XF86Paste] paste-over)
     ([M-XF86Paste] paste-rotate-reverse)
     ([M-S-XF86Paste] paste-rotate)
     ([s-XF86Paste] paste-primary)

     ;; Text-selection commands
     ([SunFront] push-mark-command) ; X calls my setmark key ⌜SunFront⌝
     ([S-SunFront] line-select-minor-mode-enable)
     ([M-SunFront] rectangle-mark-mode)
     ([M-S-SunFront] mark-whole-buffer))))


(provide 'vimizer)

;;; vimizer.el ends here
