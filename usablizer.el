;;; usablizer.el --- Make Emacs usable -*- lexical-binding: t; -*-
;; Version: 0.1
;; Package-Requires: ((undo-tree "0.6.5") (vimizer "0.1"))
;; Keywords: convenience

;; This file doesn't use hard word wrap. To fold away the long comments and docstrings, use:
;; (setq truncate-lines t)
;; or with this file's functions, use:
;; (set-line-wrap 'off) ; or press M-S-right to cycle to that setting
;; To show the long lines, use:
;; (set-line-wrap 'word) ; or press M-S-right


;;; Commentary:

;;; Code:

(require 'misc)
(require 'undo-tree)
(eval-when-compile (require 'cl))
(eval-and-compile (require 'vimizer)) ; For the «silently» macro, and global-set-key-list

(defalias 'isearch-truncate-or-abort 'isearch-abort) ; See isearch-really-abort


;;; Utilities

;; Add this function to find-file-hook to prevent your positions stored in registers from becoming stale when you edit a file after closing and reopening it.
(defun register-swap-back () ; This ought to be in register.el
  "Turn file-query references back into markers when a file is visited."
  (dolist (elt register-alist)
    (when (and (consp (cdr elt))
	       (eq (cadr elt) 'file-query)
	       (equal (caddr elt) buffer-file-name))
      (setcdr elt (copy-marker (cadddr elt)))
      (add-hook 'kill-buffer-hook 'register-swap-out nil t))))

;; Make proper error message for sexp-based movement commands
(defun sexp-error-message (arg nominal-direction)
  (let ((absarg (abs arg))
	(real-direction-word
	 (if (> (* arg nominal-direction) 0) "further" "previous")))
    (if (= absarg 1) (format "No %s sexp" real-direction-word)
      (format "There aren't %d %s sexps" absarg real-direction-word))))

(defun line-wrap-disable ()
  (if truncate-partial-width-windows ; Don't bother with buffer-local if global is nil
      (setq-local truncate-partial-width-windows nil)) ; Don't interfere with other buffers' settings
  ;; These two are automatically buffer-local
  (setq truncate-lines t)
  (setq word-wrap nil))

(defun char-wrap-enable ()
  (if truncate-partial-width-windows
      (setq-local truncate-partial-width-windows nil))
  (setq truncate-lines nil)
  (setq word-wrap nil))

(defun word-wrap-enable ()
  (if truncate-partial-width-windows
      (setq-local truncate-partial-width-windows nil))
  (setq truncate-lines nil)
  (setq word-wrap t))

(defun get-line-wrap () ; Ignores truncate-partial-width-windows
  (cond (truncate-lines 'off)
	(word-wrap 'word)
	(t 'char)))

;; Change a setting only if the user hasn't already customized it
(defun maybe-set (sym val)
  "If SYM is currently set to its standard value, then set SYM to VAL and return VAL.
Otherwise, return nil."
  (if (equal (symbol-value sym)
	     (eval (car (get sym 'standard-value))))
      (set sym val)))


;;; Replace Emacs's point-losing pop-to-mark-command

(defvar rotate-mark-ring-hook nil
"Run after `reverse-rotate-mark-ring-and-point' and `rotate-mark-ring-and-point'.")

;; Derived from pop-mark and pop-to-mark-command in simple.el
(defun reverse-rotate-mark-ring-and-point ()
  "Rotate mark ring, mark, and point in reverse chronological order.
That is, save point to tail of mark ring, jump to mark, and pop a new position for mark off the ring. (Does not affect global mark ring.)

This command is designed to replace the standard `pop-to-mark-command', which loses the current point.

Unlike `pop-to-mark-command', doesn't message ⌜Mark popped⌝ or deactivate the mark, because those are annoying. If you want them, add them to `rotate-mark-ring-hook'.

Runs `rotate-mark-ring-hook' afterward.

See also `rotate-mark-ring-and-point', which rotates in chronological order."
  (interactive)
  (if (null (mark t)) (user-error "No mark set in this buffer"))
  (setq mark-ring (nconc mark-ring (list (point-marker))))
  (goto-char (mark t))
  (set-marker (mark-marker) (marker-position (car mark-ring)))
  (set-marker (car mark-ring) nil)
  (setq mark-ring (cdr mark-ring))
  (run-hooks 'rotate-mark-ring-hook))

(defun rotate-mark-ring-and-point ()
  "Like `reverse-rotate-mark-ring-and-point', but in chronological order.

Runs `rotate-mark-ring-hook' afterward."
  (interactive)
  (if (null (mark t)) (user-error "No mark set in this buffer"))
  (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
  (set-marker (mark-marker) (point-marker))
  ;; Lisp requires special-casing tail-chopping, unlike head-chopping
  (let* ((last2 (if (> (length mark-ring) 1) (last mark-ring 2) nil))
	 (last1 (if last2 (cdr last2) mark-ring)))
    (goto-char (car last1))
    (set-marker (car last1) nil)
    (if last2 (setcdr last2 nil)
      (setq mark-ring nil)))
  (run-hooks 'rotate-mark-ring-hook))

(defvar mark-ring-position nil
  "Current position of point in the full mark ring (including mark and point).
Reset at the beginning of each sequence of consecutive calls to `reverse-rotate-mark-ring-and-point' and `rotate-mark-ring-and-point'.

Used only as state for `track-mark-ring-position'.")
(make-variable-buffer-local 'mark-ring-position)

(defun track-mark-ring-position ()
  "Track navigation through the mark ring.
When added to `rotate-mark-ring-hook', update `mark-ring-position' after `reverse-rotate-mark-ring-and-point' and `rotate-mark-ring-and-point', and message when user rotates back to start position, for the same reason that isearch messages ⌜wrapped⌝."
  (unless (and mark-ring-position
	       (or (eq last-command 'reverse-rotate-mark-ring-and-point)
		   (eq last-command 'rotate-mark-ring-and-point)))
    (setq mark-ring-position 0))
  (let ((size (+ 2 (length mark-ring)))) ; Point and mark count too, so +2
    (setq mark-ring-position
	  (mod (+ mark-ring-position
		  (if (eq this-command 'rotate-mark-ring-and-point) 1 -1)) size))
    (if (= mark-ring-position 0) (message "Back to start; ring size=%d" size))))

(defun clean-mark-ring ()
  "Clear null markers from `mark-ring'.
If `mark' is nil but there's at least one non-nil marker on the ring, move it to mark. And truncate `mark-ring' if its length exceeds `mark-ring-max'. In these cases, there's a bug somewhere. This function sweeps such bugs under the rug. However, no such bugs are suspected to exist."
  ;; Chop null markers from head
  (while (and mark-ring (not (marker-position (car mark-ring)))
	      (setq mark-ring (cdr mark-ring))))
  ;; Clean them out from the rest of the list
  (let ((tail mark-ring))
    (while tail
      (if (and (cadr tail) (not (marker-position (cadr tail))))
	  (setcdr tail (cddr tail)))
      (setq tail (cdr tail))))
  ;; Make sure mark is valid if there's anything on the ring
  (when (and mark-ring (null (mark t)))
    (set-marker (mark-marker) (marker-position (car mark-ring)))
    (set-marker (car mark-ring) nil)
    (setq mark-ring (cdr mark-ring)))
  ;; Set excess markers to nil, then truncate the tail
  (let* ((tail (nthcdr (1- mark-ring-max) mark-ring))
	 (truncatees (cdr tail)))
    (when tail
      (while truncatees
	(set-marker (car truncatees) nil)
	(setq truncatees (cdr truncatees)))
      (setcdr tail nil))))


;;; Movement commands

(defun back-sexp (&optional arg)
  "Do `backward-sexp' but report more user-friendly errors."
  (interactive "p")
  (unless arg (setq arg 1))
  (condition-case nil (backward-sexp arg)
    ('scan-error (user-error (sexp-error-message arg -1)))))

(defun end-sexp (&optional arg)
  "Do `forward-sexp' but report more user-friendly errors."
  (interactive "p")
  (unless arg (setq arg 1))
  (condition-case nil (forward-sexp arg)
    ('scan-error (user-error (sexp-error-message arg 1)))))

;; This dance is demented, but it works, and I can't find a shorter one that does
(defun forward-to-sexp (&optional arg)
  "Move forward to beginning of next sexp."
  (interactive "p")
  (unless arg (setq arg 1))
  (if (< arg 1) (end-sexp arg) ; Backward (and zero) movement are as usual
    (let ((start (point)))
      (let ((target
	     (condition-case nil
		 (save-excursion ; And it's an excursion indeed...
		   (forward-sexp 1)
		   (forward-sexp -1)
		   (unless (> (point) start) (forward-sexp 1)) ; Get to the right point regardless of whether the preceding oscillation crossed the start of the next sexp
		   (forward-sexp arg)
		   (forward-sexp -1)
		   (max (point) start)) ; Don't screw up if at end of file and there are no more sexps (forward-sexp doesn't signal an error in this case)
	       ('scan-error (user-error (sexp-error-message arg 1))))))
	(goto-char target))))) ; Excursing and then gotoing allows forward-sexp to signal an error and prevent any movement at all. Otherwise, if forward-sexp signals an error in the middle of the dance, point would be left at an unintended intermediate location.

(defun not-weird-beginning-of-defun (&optional arg)
  "Do `beginning-of-defun', but don't pretend that 0=1.
If ARG is 0, then don't do anything."
  (interactive "p")
  (unless (= arg 0)
    (if (called-interactively-p 'any)
	(setq this-command 'beginning-of-defun)) ; So it pushes the mark
    (beginning-of-defun arg)))

(defun not-weird-end-of-defun (&optional arg)
  "Do `end-of-defun', but without its weird behavior.
Leave point at the end of the defun where it ought to be, rather than at the beginning of the next line. And if point is already at the end of a defun, then actually move to the end of the next defun, rather than just moving to the start of the next line."
  (interactive "p")
  (unless arg (setq arg 1))
  (unless (= arg 0)
    (if (< arg 0) (beginning-of-defun (- arg))
      (let ((start (point)))
	(condition-case e
	    (progn ; Dance like forward-to-sexp
	      (end-of-defun)
	      (backward-sexp)
	      (forward-sexp)
	      ;; Now we're at start if it's at end of defun, or before start if it's after end of last defun in buffer. Therefore:
	      (if (<= (point) start) (forward-sexp))
	      ;; Now we've accomplished what (end-of-defun 1) would have accomplished in the first place, if it weren't weird. Finally:
	      (forward-sexp (1- arg))
	      ;;Push mark here instead of before moving, in case movement signals scan-error
	      ;; Derived from beginning-of-defun in lisp.el
	      (or (not (eq this-command 'not-weird-end-of-defun))
		  (eq last-command 'not-weird-end-of-defun)
		  (and transient-mark-mode mark-active)
		  (push-mark start)))
	  ('scan-error
	   (goto-char start)
	   (user-error (cadr e))))))))

(defun forward-to-defun (&optional arg)
  "Move to the beginning of the next defun, analogous to `forward-to-sexp'."
  (interactive "p")
  (if (called-interactively-p 'any) (setq this-command 'beginning-of-defun)) ; So it pushes the mark
  ;; XXX: Would need to do same dance as for forward-to-paragraph and forward-to-sexp, which would mean certainly now I need to put that dance into a macro, instead of repeating the code, but fortunately beginning-of-defun moves to next start of defun rather than to next end of defun.
  (not-weird-beginning-of-defun (- arg)))

(defun not-weird-backward-paragraph (&optional arg)
  "Like `backward-paragraph', but not weird.
Land on start of paragraph, like backward-word lands on start of word."
  (interactive "p")
  (unless arg (setq arg 1))
  (if (< arg 0) (forward-to-paragraph (- arg)))
  (if (> arg 0)
      (let ((start (point)))
	(backward-paragraph)
	(if (= (1+ (point)) start) (backward-paragraph))
	(if arg (backward-paragraph (1- arg)))
	(if (looking-at "\n[^\n]") (forward-char)))))

(defun not-weird-forward-paragraph (&optional arg)
  "Like `forward-paragraph', but not weird when given negative argument.
Land after end of paragraph when going forward, like `forward-word' lands after end of word, and land on start of paragraph when going backward (due to negative argument), like forward-word lands on start of word with negative argument."
  (interactive "p")
  (if (and arg (< arg 0)) (not-weird-backward-paragraph (- arg))
    (forward-paragraph arg)))

(defun forward-to-paragraph (&optional arg)
  "Move to beginning of next paragraph.
(Unlike `forward-paragraph', which moves to end of paragraph.) Land on start of paragraph, like `forward-to-word' lands on start of word."
  (interactive "p")
  (unless arg (setq arg 1))
  (if (< arg 0) (not-weird-backward-paragraph (- arg)))
  (if (> arg 0)
      (let ((start (point)))
	(let ((target (save-excursion ; Dance like forward-to-sexp
			(not-weird-forward-paragraph)
			(not-weird-backward-paragraph)
			(unless (> (point) start) (not-weird-forward-paragraph))
			(not-weird-forward-paragraph arg)
			(not-weird-backward-paragraph)
			(if (> (point) start) (point) start))))
	  (goto-char target)))))

(defun backward-out-list (&optional arg)
  "Do `backward-up-list' but report more user-friendly error."
  (interactive "p")
  (condition-case nil (backward-up-list arg)
    ('scan-error (user-error "No outer list"))))

(defun out-list (&optional arg)
  "Do `up-list' but report more user-friendly error."
  (interactive "p")
  (condition-case nil (up-list arg)
    ('scan-error (user-error "No outer list"))))

(defun in-list (&optional arg)
  "Do `down-list' but report more user-friendly error."
  (interactive "p")
  (condition-case nil (down-list arg)
    ('scan-error (user-error "No inner list"))))

(defun home-list (&optional level)
  "Go to home of current list.
With optional LEVEL, go backward to home of nth containing list (default 1st)."
  (interactive "p")
  (if (and level (< level 0)) (end-list (- level))
    (condition-case nil (progn (backward-up-list level) (right-char))
      ('scan-error (user-error (if (and level (> level 1))
				   (format "Not in %d lists" level) "Not in list"))))))

(defun end-list (&optional level)
  "Go to end of current list.
With optional LEVEL, go forward to end of nth containing list (default 1st)."
  (interactive "p")
  (if (and level (< level 0)) (home-list (- level))
    (condition-case nil (progn (up-list level) (left-char))
      ('scan-error (user-error (if (and level (> level 1))
				   (format "Not in %d lists" level) "Not in list"))))))

(defun silent-beginning-of-buffer (arg)
  (interactive "P")
  (silently (with-no-warnings ; Ugh
	      (beginning-of-buffer arg))))

(defun silent-end-of-buffer (arg)
  (interactive "P")
  (silently (with-no-warnings
	      (end-of-buffer arg))))

;; Derived from Emacs's zap-to-char
;; TODO: inclusive and exclusive find are implemented differently, for no good reason. Choose the better one, and do both that way.
(defun find-char-inclusive (arg char)
  "Find ARGth occurrence of CHAR and leave point after it, or at it if backward search.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found."
  (interactive (list (prefix-numeric-value current-prefix-arg)
		     (read-char "Find char inclusive: " t)))
  ;; Avoid ⌜obsolete⌝ warnings for translation-table-for-input
  (with-no-warnings
    (if (char-table-p translation-table-for-input)
	(setq char (or (aref translation-table-for-input char) char))))
  (let ((start (point)))
    (search-forward (char-to-string char) nil nil arg)
    (if (and (not mark-active) (not (eq (point) start))) (push-mark start))))

(defun backward-find-char-inclusive (arg)
  (interactive "P")
  (setq current-prefix-arg (- (prefix-numeric-value arg)))
  (call-interactively #'find-char-inclusive))

;; Derived from Emacs's zap-up-to-char
(defun find-char-exclusive (arg char)
  "Find ARGth occurrence of CHAR and leave point at it, or after it if backward search.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found.
Ignores CHAR at point."
  (interactive "p\ncFind char exclusive: ")
  (let ((direction (if (>= arg 0) 1 -1))
	(start (point)))
    (forward-char direction)
    (unwind-protect
	(search-forward (char-to-string char) nil nil arg)
      (backward-char direction))
    (if (and (not mark-active) (not (eq (point) start))) (push-mark start))))

(defun backward-find-char-exclusive (arg)
  (interactive "P")
  (setq current-prefix-arg (- (prefix-numeric-value arg)))
  (call-interactively #'find-char-exclusive))

(defun other-window-back (&optional arg)
  "Same as `other-window', but with negated arg. If no arg, then use -1 by default."
  (interactive "p") (other-window (if arg (- arg) -1)))


;;; Editing commands

(defun backward-delete-word (&optional arg)
  "Don't put deleted word into clip ring, unlike Emacs's default `backward-kill-word'.
This is to avoid cluttering the clip ring when deleting typos."
  (interactive "p")
  (save-excursion ; Don't move point if delete-region signals read-only
    (let ((start (point)))
      (backward-word (or arg 1))
      (delete-region start (point)))))

(defun insert-tab-command (&optional arg)
  "Insert ARG tab chars (default 1)."
  (interactive "P")
  (let ((indent-tabs-mode t))
    (insert-tab arg)))

(defun indent-sexp-or-region (&optional arg)
  "Indent sexp via `indent-pp-sexp', or indent region rigidly if active.
With prefix arg, pretty-print."
  (interactive "P")
  (if (use-region-p) (call-interactively #'indent-rigidly)
    (condition-case e (indent-pp-sexp arg) ; scan-sexps signals error, not user-error
      ('scan-error (user-error (cadr e))))))

(defun indent-defun-or-region (&optional arg)
  "Indent current defun via `indent-pp-sexp', or indent region rigidly if active.
Instead indent current top-level sexp if `defun-prompt-regexp' and `open-paren-in-column-0-is-defun-start' are both nil.
With prefix arg, pretty-print."
  (interactive "P")
  (if (use-region-p) (call-interactively #'indent-rigidly)
    (save-excursion
      (condition-case e ; scan-sexps signals error, not user-error
	  (progn (end-of-defun)
		 (backward-sexp)
		 (indent-pp-sexp arg))
	('scan-error (user-error (cadr e)))))))

(defun nl-under (&optional arg)
  "Vim's «o»."
  (interactive "p")
  (unless arg (setq arg 1))
  (if (< arg 0)
      (nl-over (- arg))
    (when (> arg 0)
      (let ((start (point)))
	(condition-case nil (progn
			      (move-end-of-line nil)
			      (newline arg)
			      (indent-according-to-mode))
	  ;; Derived from kill-region in simple.el
	  ((buffer-read-only text-read-only)
	   (goto-char start) ; Don't move point if the edit signals read-only
	   (barf-if-buffer-read-only)
	   (signal 'text-read-only (list (current-buffer)))))))))

(defun nl-over (&optional arg)
  "Vim's «O»."
  (interactive "p")
  (unless arg (setq arg 1))
  (if (< arg 0)
      (nl-under (- arg))
    (when (> arg 0)
      (let ((start (point)))
	(condition-case nil (progn
			      (move-beginning-of-line nil)
			      (newline arg)
			      (forward-line (- arg))
			      (indent-according-to-mode))
	  ;; Derived from kill-region in simple.el
	  ((buffer-read-only text-read-only)
	   (goto-char start) ; Don't move point if the edit signals read-only
	   (barf-if-buffer-read-only)
	   (signal 'text-read-only (list (current-buffer)))))))))


;;; Miscellaneous commands

(defun eval-region-or-last-sexp (&optional arg)
  "Eval region if active; otherwise, eval last sexp.
See `eval-region' and `eval-last-sexp' for details.

Print value into echo area or, with prefix argument, into current buffer. When printing into current buffer, honor `delete-active-region', thus overwrite region if active and delete-active-region is non-nil, unless ARG is negative, in which case ignore delete-active-region and don't overwrite active region."
  (interactive "P")
  (if mark-active
      ;; Use markers instead of just positions, since evaling the region could cause insertion or deletion of text in front of the region, and subsequent deletion of the region or insertion of eval-region's output relative to the region requires knowledge of where the region moved to.
      (let* ((start (copy-marker (region-beginning) t))
	     (end (copy-marker (region-end)))
	     (orig-point (copy-marker (point)))
	     (raw-arg (if (consp arg) (car arg) arg))
	     (do-delete (and (integerp raw-arg) (>= raw-arg 0)))) ; ⌜>=⌝ because docstring says so
	(if (= start end) (deactivate-mark) ; Nothing to evaluate
	  (if arg
	      (progn
		(eval-region start end (current-buffer))
		(if do-delete (cond
			       ((eq delete-active-region 'kill) (not-presumptuous-cut-region start end))
			       (delete-active-region (delete-region start end))))
		(if (looking-back "\n" (- (point) 1))
		    (delete-char -1)) ; Get rid of superfluous eol inserted by eval-region, to be consistent with eval-last-sexp, which doesn't insert it
		(let ((new-end (point)))
		  (goto-char orig-point)
		  (when (looking-at "\n")
		    (delete-char 1) ; Get rid of other superfluous eol inserted by eval-region
		    (setq new-end (1- new-end)))
		  (goto-char new-end)
		  (set-marker start nil)
		  (set-marker end nil)
		  (set-marker orig-point nil)))
	    (eval-region start end t))))
    (if arg (setq this-command 'eval-last-sexp)) ; Work around strangeness in eval-expression-print-format
    (eval-last-sexp arg)))

;; Derived from http://emacsredux.com/blog/2013/05/04/rename-file-and-buffer/
(defun rename-file-and-buffer ()
  "Rename current buffer and the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file")
      (let* ((new-name (read-file-name "New name: " filename))
	     (x (find-buffer-visiting new-name)))
        (cond
	 (x (if (eq x (current-buffer))
		(message "Source and destination are the same")
	      ;; Avoid horrible accidents
	      (let ((conflict (expand-file-name (buffer-file-name x))))
		(message "%S is already visiting %s%s" x new-name
			 (if (string= (expand-file-name new-name) conflict)
			     "" (format " under name %S" conflict))))))
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t (if (and (file-exists-p new-name) ; And other horrible accidents
		     (not (y-or-n-p (format
				     "%s already exists; overwrite? "
				     new-name))))
		(message "Canceled rename")
	      (rename-file filename new-name t)
	      (set-visited-file-name new-name t t)
	      (message "Renamed %s to %S" filename new-name))))))))

;; Derived from simple.el
(defun not-annoying-keyboard-escape-quit ()
  "The same as `keyboard-escape-quit', but never delete windows and never bury the buffer."
  (interactive)
  (cond ((eq last-command 'mode-exited) nil)
	((region-active-p)
	 (deactivate-mark))
	((> (minibuffer-depth) 0)
	 (abort-recursive-edit))
	(current-prefix-arg
	 nil)
	((> (recursion-depth) 0)
	 (exit-recursive-edit))
	(buffer-quit-function
	 (funcall buffer-quit-function))))

(defun isearch-really-abort ()
  "Abort incremental search mode, signaling quit.
This function ought to be called ⌜isearch-abort⌝, but that name is already taken (in Emacs 24.2) by `isearch-abort' in isearch.el (from which this function is derived), which ought to be called ⌜isearch-truncate-or-abort⌝ since that's what it does (truncate nonmatching tail of search string, or abort if that tail is empty)."
  (interactive)
  (discard-input)
  (setq isearch-success nil)
  (isearch-cancel))

;; Normally, partial escape is same as full escape, but making it a separate function enables proper remapping of keys, e.g. for isearch mode
(defun partial-escape ()
  (interactive)
  (keyboard-quit))

(defun set-line-wrap (&optional arg)
  "Set line wrap for lines longer than the window width.
If ARG is omitted or nil, cycle no wrapping, character wrapping, and word wrapping.

If ARG is 'off, 'char, or 'word, then set line wrapping accordingly, regardless of current setting.

This command sets the values of `truncate-lines' and `word-wrap', which are separate variables in Emacs in order to confuse you. In order to ensure that this command works as advertised, it also buffer-locally disables `truncate-partial-width-windows'."
  (interactive)
  (cond ((eq arg 'off) (line-wrap-disable))
	((eq arg 'char) (char-wrap-enable))
	((eq arg 'word) (word-wrap-enable))
	(truncate-lines (char-wrap-enable))
	(word-wrap (line-wrap-disable))
	(t (word-wrap-enable)))
  (force-mode-line-update))

(defun toggle-region-activation ()
  (interactive)
  (if (mark t)
      (if mark-active (deactivate-mark) (activate-mark))
    (user-error "No mark set in this buffer")))

(defun narrow-to-region-tweaked ()
  "Run `narrow-to-region', then deactivate mark."
  (interactive)
  (narrow-to-region (point) (mark))
  (deactivate-mark))

(defun not-hijacked-kill-buffer ()
  "Do `kill-buffer'.
Bind a key to this function instead of directly to kill-buffer so that ido-mode won't hijack the keybinding."
  (interactive)
  (kill-buffer))

;; Copied from http://stackoverflow.com/questions/1212426/how-do-i-close-an-automatically-opened-window-in-emacs
(defun other-window-kill-buffer ()
  "Kill the buffer in the other window"
  (interactive)
  ;; Window selection is used because point goes to a different window
  ;; if more than 2 windows are present
  (let ((win-curr (selected-window))
        (win-other (next-window)))
    (select-window win-other)
    (kill-this-buffer)
    (select-window win-curr)))

(defun undo-tree-mode-not-enabled ()
  (interactive)
  (user-error "undo-tree-mode not enabled"))

(defun flyspell-mode-toggle ()
  "Toggle `flyspell-mode' without superfluous message'." ; Mode is already indicated on the mode line
  (interactive)
  (flyspell-mode (if flyspell-mode 0 t)))

;; Copied from http://ergoemacs.org/emacs/modernization_upcase-word.html
;; Fixed Xah's weird formatting, and replaced his string constants by symbols
(defun toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
Toggles between: “all lower”, “Init Caps”, “ALL CAPS”."
  (interactive)
  (let (p1 p2 (deactivate-mark nil) (case-fold-search nil))
    (if (region-active-p)
        (setq p1 (region-beginning) p2 (region-end))
      (let ((bds (bounds-of-thing-at-point 'word)))
        (setq p1 (car bds) p2 (cdr bds))))
    (when (not (eq last-command this-command))
      (save-excursion
        (goto-char p1)
        (cond
         ((looking-at "[[:lower:]][[:lower:]]") (put this-command 'state 'all-lower))
         ((looking-at "[[:upper:]][[:upper:]]") (put this-command 'state 'all-caps))
         ((looking-at "[[:upper:]][[:lower:]]") (put this-command 'state 'init-caps))
         ((looking-at "[[:lower:]]") (put this-command 'state 'all-lower))
         ((looking-at "[[:upper:]]") (put this-command 'state 'all-caps))
         (t (put this-command 'state 'all-lower)))))
    (cond
     ((eq 'all-lower (get this-command 'state))
      (upcase-initials-region p1 p2) (put this-command 'state 'init-caps))
     ((eq 'init-caps (get this-command 'state))
      (upcase-region p1 p2) (put this-command 'state 'all-caps))
     ((eq 'all-caps (get this-command 'state))
      (downcase-region p1 p2) (put this-command 'state 'all-lower)))))

;; Following doesn't work exactly as I want, since argument of zero won't delete all spaces and newlines:
;; (defun just-one-space-tweaked (&optional n)
;;   "Same as `just-one-space' but with argument negated, since defaulting to deleting newlines is better."
;;   (interactive "*p")
;;   (just-one-space (if n (- n) -1)))

;; So I had to copy and tweak it to get it exactly right
;; Derived from just-one-space from simple.el
;; FIXME: The description ⌜If N is negative, don't delete newlines⌝ appears to be wrong; the function takes abs of n. I'm not bothering to fix this, since I've never missed that feature.
(defun just-one-space-tweaked (&optional n)
  "Delete all spaces, tabs, and newlines around point, leaving one space (or N spaces).
If N is negative, don't delete newlines."
  (interactive "*p")
  (unless n (setq n 1))
  (let ((orig-pos (point))
        (skip-characters (if (< n 0) " \t" " \t\n\r"))
        (n (abs n)))
    (skip-chars-backward skip-characters)
    (constrain-to-field nil orig-pos)
    (dotimes (_i n) ; Marking «i» so the byte compiler doesn't whine
      (if (= (following-char) ?\s)
	  (forward-char 1)
	(insert ?\s)))
    (delete-region
     (point)
     (progn
       (skip-chars-forward skip-characters)
       (constrain-to-field nil orig-pos t)))))

;; And it turns out I'd rather just toggle between one space and one eol (cycle-spacing doesn't do what I want). Toggling design derived from toggle-letter-case at http://ergoemacs.org/emacs/modernization_upcase-word.html
(defun just-one-space-or-eol (&optional arg)
  "Delete all spaces, tabs, and newlines around point, leaving one eol, or with prefix arg, leave nothing. If repeated, toggle between one space and one eol char."
  (interactive "P")
  (if arg (just-one-space-tweaked 0)
    (if (and (eq this-command last-command) ; Could be false due to calling with prefix arg, then calling without
	     (looking-back "[\n ]" (1- (point))))
	(progn
	  (delete-char -1)
	  (case (get this-command 'state)
	    ('space (progn (put this-command 'state 'eol)
			   (insert ?\n)))
	    ('eol (progn (put this-command 'state 'space)
			 (insert ?\s)))
	    (t (error "Nooo!"))))
      (put this-command 'state 'space)
      (just-one-space-tweaked))))

(defun remove-all-text-properties ()
  "Remove all text properties from active region, or from inactive region following yank."
  (interactive)
  (if (or mark-active (eq last-command 'yank))
      (set-text-properties (point) (mark) nil)
    ;; If neither condition is true, then this command was probably an accident.
    (user-error "Region not active, and previous command was not a yank")))


;;; Special case prefix args 2 and 3 since they're so common, and deserve their own keys without having to press the uarg key before them to initiate numeral entry. 4 is default for uarg, so it already doesn't require pressing an extra key. 5 and higher are infrequent enough that no special casing is needed; requiring uarg command prefix for them is ok.

;; Derived from simple.el
(defun universal-argument-x (&optional start)
  "Same as `universal-argument', but with optional START value instead of default of 4."
  (interactive)
  (setq prefix-arg (list (or start 4)))
  (universal-argument--mode))

;; Derived from simple.el
(defmacro define-universal-arg-funs (x)
  `(mapc
    #'eval
    ',(list `(defun ,(intern (format "universal-argument-%d" x)) ()
	       (interactive)
	       (universal-argument-x ,x))
	    `(defun ,(intern (format "universal-argument-more-%d" x)) (arg)
	       (interactive "P")
	       (setq prefix-arg (if (consp arg)
				    (list (* ,x (car arg)))
				  (if (eq arg '-)
				      (list (- ,x))
				    arg)))
	       (when (consp prefix-arg) (universal-argument--mode))))))

(define-universal-arg-funs 2)
(define-universal-arg-funs 3)
;; 4 not needed here, since already hardcoded in simple.el


;;; Init

;; This is an essential usability issue, so I'm putting it at top level, not in a function that must be run.
(add-hook 'find-file-hook 'register-swap-back)

(defun usablizer-bind-keys ()
  "Hijack the user's keybindings."
  (unless line-move-visual
    (user-error "Aborting usablizer-bind-keys to avoid overriding your weirdo config"))

  (setq shift-select-mode nil) ; The Windintosh junk

  ;; Get rid of annoying new global keybindings in Emacs 24.3
  (global-unset-key [XF86Back]) ; By default bound to previous-buffer
  (global-unset-key [XF86Forward]) ; next-buffer

  (global-set-key-list
   '(

     ;;Movement commands
     ([S-up] previous-logical-line) ; Non-shifted up and down move by visual lines
     ([S-down] next-logical-line)
     ([S-prior] scroll-down-line)
     ([S-next] scroll-up-line)
     ([f24] forward-to-word)
     ([C-f15] reverse-rotate-mark-ring-and-point) ; Using C-15 for revmark key since I'm out of available scancodes
     ([C-S-f15] rotate-mark-ring-and-point)
     ([C-S-left] back-sexp) ; Using C-left and C-right for bw_word and end_wrd keys, so I get S-bw_word and S-end_wrd for back-sexp and end-sexp
     ([C-S-right] end-sexp)
     ([S-f24] forward-to-sexp)
     ([C-M-S-left] not-weird-beginning-of-defun)
     ([C-M-S-right] not-weird-end-of-defun)
     ([M-S-f24] forward-to-defun)
     ([M-up] not-weird-backward-paragraph)
     ([M-S-down] not-weird-forward-paragraph)
     ([M-down] forward-to-paragraph)
     ([C-M-left] backward-out-list)
     ([C-M-right] out-list)
     ([M-f24] in-list)
     ([S-home] home-list)
     ([S-end] end-list)
     ([M-S-home] beginning-of-visual-line)
     ([M-S-end] end-of-visual-line) ; FIXME (Emacs bug): works if word wrap enabled, but moves one too many chars if char wrap enabled and font is monospace.
     ([M-home] silent-beginning-of-buffer)
     ([M-end] silent-end-of-buffer)
     ;; Exclusive forward find-char paired with inclusive backward is intentional
     ([f21] find-char-exclusive)
     ([S-f21] backward-find-char-inclusive)
     ([M-f21] find-char-inclusive)
     ([M-S-f21] backward-find-char-exclusive)
     ([M-S-up] move-to-window-line-top-bottom)
     ([S-right] recenter-top-bottom)

     ;;Editing commands
     ([S-backspace] backward-delete-word)
     ([S-delete] just-one-space-or-eol)
     ([M-delete] remove-all-text-properties)
     ([f23] indent-for-tab-command)
     ([S-f23] insert-tab-command)
     ([C-f23] indent-sexp-or-region)
     ([C-S-f23] indent-defun-or-region)
     ([S-return] electric-indent-just-newline)
     ([M-return] nl-under)
     ([M-f22] nl-under)
     ([M-S-return] nl-over)
     ([M-S-f22] nl-over)
     ([f16] toggle-letter-case)
     ([S-f16] ispell-word)
     ([M-f16] ispell)
     ([M-S-f16] flyspell-mode-toggle)

     ;; File, buffer, and window management
     ([SunOpen] switch-to-buffer) ; ido-mode remaps to ido-switch-buffer
     ([S-SunOpen] ido-find-file)
     ([M-SunOpen] ibuffer)
     ([M-S-SunOpen] bury-buffer)
     ([XF86Close] not-hijacked-kill-buffer)
     ([S-XF86Close] reopen-buffer)
     ([M-XF86Close] other-window-kill-buffer)
     ([s-XF86Close] kill-buffer-and-window)
     ([XF86Save] save-buffer)
     ([S-XF86Save] save-some-buffers)
     ([s-up] windmove-up)
     ([s-down] windmove-down)
     ([s-left] windmove-left)
     ([s-right] windmove-right)
     ([C-tab] other-window)
     ([C-S-iso-lefttab] other-window-back) ; Emacs calls S-tab ⌜S-iso-lefttab⌝
     ([s-prior] split-window-vertically)
     ([s-next] delete-other-windows-vertically)
     ([C-s-left] split-window-horizontally)
     ([C-s-right] delete-other-windows-horizontally) ; TODO: implement this
     ([s-delete] delete-window)
     ([s-f24] delete-other-windows) ; xmonad uses s-space, not s-f24
     ([s-undo] winner-undo) ; TODO: replace by better variant in workgroups2
     ([s-S-undo] winner-redo)

     ;; Miscellaneous
     ([M-S-f15] count-words)
     ([M-S-right] set-line-wrap)
     ([f18] universal-argument)
     ([S-f18] universal-argument)
     ([M-f18] negative-argument) ; XXX: M-f18 n where n is a numeral produces the negative, as expected, unless n is 0, in which case it produces -1, due to brain damage in Emacs's universal argument processing (see digit-argument in simple.el)
     ([M-S-f18] negative-argument)
     ([f2] universal-argument-2)
     ([S-f2] universal-argument-2)
     ([f3] universal-argument-3)
     ([S-f3] universal-argument-3)
     ([menu] menu-bar-open)
     ([C-menu] execute-extended-command) ; Using C-menu for execmd key since I'm out of available scancodes
     ([C-S-menu] eval-region-or-last-sexp)
     ([C-M-menu] eval-expression)
     ([C-M-S-menu] shell-command)
     ;; Disable undo key if undo-tree mode is disabled, to avoid accidentally using Emacs's standard undo without realizing it
     ([undo] undo-tree-mode-not-enabled)
     ([S-undo] undo-tree-mode-not-enabled)
     ([M-undo] undo-tree-mode-not-enabled)
     ([M-S-undo] revert-buffer)
     ([f20] exchange-point-and-mark)
     ([S-f20] toggle-region-activation) ; I had this as narrow-to-region-tweaked, but I kept accidentally hitting while holding shift to move by lines.
     ([M-f20] narrow-to-region-tweaked)
     ([M-S-f20] widen)
     ([f19] jump-to-register)
     ([S-f19] point-to-register)
     ([M-f19] list-registers)
     ;; TODO change scrolling for undo-tree visualizer to use scroll-lock-mode, or at least stop scrolling conservatively. Just setting scroll-conservatively with let binding doesn't work; global value has to be set. Maybe using make-local-variable?
     ([Scroll_Lock] scroll-lock-mode))) ; FIXME (Emacs bug): scroll-lock-mode doesn't work right on wrapped lines; point gets dragged. And scroll-lock-mode doesn't work in undo-tree visualizer.

  (mapc
   (lambda (x) (define-key universal-argument-map (car x) (cadr x)))
   '(([f18] universal-argument-more)
     ([S-f18] universal-argument-more)
     ([f2] universal-argument-more-2)
     ([S-f2] universal-argument-more-2)
     ([f3] universal-argument-more-3)
     ([S-f3] universal-argument-more-3)))

  (define-key undo-tree-map [undo] 'undo-tree-undo)
  (define-key undo-tree-map [S-undo] 'undo-tree-redo)
  (define-key undo-tree-map [M-undo] 'undo-tree-visualize)


;;; Emacs: the customizable text editor, in roughly the same way that a brick wall is customizable. If you beat your head against it hard enough, you can actually shove it into a less obstructive form.

  ;;Partially fix the «escape» key
  ;;FIXME (Emacs inanity): This doesn't cover all the cases. Emacs needs surgery to get rid of all the hardcoded C-g
  (global-set-key [escape] 'keyboard-quit) ; Default binding of esc key is esc-map, which is ridiculous
  (global-set-key [S-escape] 'partial-escape)
  (global-set-key [M-escape] 'not-annoying-keyboard-escape-quit)
  (global-set-key [M-S-escape] esc-map) ; In case esc-map is actually needed for something (unlikely)
  ;; The following is the proper way to do mode-specific escaping, which is why Emacs doesn't do it this way by default
  (define-key isearch-mode-map [remap keyboard-quit] 'isearch-really-abort)
  (define-key isearch-mode-map [remap partial-escape] 'isearch-truncate-or-abort)
  (define-key undo-tree-visualizer-mode-map [remap keyboard-quit] 'undo-tree-visualizer-quit)
  ;; ...and some more (these ones derived from delsel.el)
  (mapc (lambda (x)
	  (define-key x [remap keyboard-quit] 'minibuffer-keyboard-quit))
	(list
	 minibuffer-local-map
	 minibuffer-local-ns-map
	 minibuffer-local-completion-map
	 minibuffer-local-must-match-map
	 minibuffer-local-isearch-map))

  ;; FIXME: I want to send escape in term mode rather than interpret it in Emacs.
  ;; But the following doesn't work, and I don't know why:
  ;; (define-key term-mode-map [escape] 'term-send-raw-meta)

  ;; Use the «find» key for isearch
  ;; Can't do (global-set-key [find] "\C-s") because as http://www.xandr.net/.emacs says:
  ;;	 it does not currently work to say
  ;;	   (global-set-key 'f3 "\C-x\C-f")
  ;;	 The reason is that macros can't do interactive things properly.
  ;;	 This is an extremely longstanding bug in Emacs.
  ;; Yay, Emacs.
  (global-set-key [find] 'isearch-forward)
  (global-set-key [S-find] 'isearch-backward)
  (define-key minibuffer-local-isearch-map [find] 'isearch-forward-exit-minibuffer)
  (define-key minibuffer-local-isearch-map [S-find] 'isearch-reverse-exit-minibuffer)
  ;; Derived from http://xahlee.org/emacs/reclaim_keybindings.html
  (define-key isearch-mode-map [find] 'isearch-repeat-forward)
  (define-key isearch-mode-map [S-find] 'isearch-repeat-backward)
  (define-key isearch-mode-map [XF86Paste] 'isearch-yank-pop))


(provide 'usablizer)

;;; usablizer.el ends here
