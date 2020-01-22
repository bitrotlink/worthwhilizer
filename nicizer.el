;;; nicizer.el --- Make Emacs nice -*- lexical-binding: t; -*-
;; Version: 0.3.31
;; Package-Requires: ((usablizer "0.4.5"))

;; This file doesn't use hard word wrap. To fold away the long comments and docstrings, use:
;; (setq truncate-lines t)
;; or with Usablizer's functions, use:
;; (set-line-wrap 'off) ; or press M-S-right to cycle to that setting
;; To show the long lines, use:
;; (set-line-wrap 'word) ; or press M-S-right


;;; Commentary:
;;
;; Nicizer improves a few display, search, and editing settings and features in Emacs. It provides:
;;
;; 0. An uncluttered modeline.
;; Minor modes that are normally on (with Nicizer, these include undo-tree-mode, ivy-mode, whitespace-mode, and word-wrap in text-mode and prog-mode buffers) have their lighters hidden in the modeline when the modes are on, and have mode-off lighters shown when the modes are off. The latter feature compensates for the former, ensuring that the modeline is never ambiguous about which modes are on.
;;
;; 1. Simpler access to the isearch history ring.
;; Instead of having to press M-p and M-n to cycle through the history ring, you can just press the up and down arrow keys, like Emacs already lets you do for cycling through various history rings in the minibuffer. Nicizer's special implementation of this feature is necessary because of Emacs's weird isearch implementation, which doesn't start by using the minibuffer even though it looks like it does.
;; After you actually do a search in isearch mode (regardless of whether you accessed the history ring), the up/down arrow key access to the history ring is automatically disabled, so you can then press up or down (or any other key for a motion command) to exit isearch mode, as usual.
;; You can still use M-p and M-n to cycle the ring, and Nicizer's simpler access feature doesn't get in your way if you don't want to use it.
;;
;; 2. A simple text-browse minor mode.
;; This is like Emacs's view-mode, except not annoying.
;;
;; 3. Variable-pitch enabled by default, and a monospace mode.
;; Since Emacs is a text editor, and text is usually more readable with variable-pitch fonts, Nicizer enables variable-pitch by default. Monospace mode is provided to compensate, and is enabled by default for major modes that need it.
;;
;; 4. Word-wrap enabled by default.
;; Sometimes you want char wrap or no wrap, but Emacs is a text editor; word wrap is the common case. For the uncommon cases, use set-line-wrap (from Usablizer).
;;
;; 5. Various Emacs settings and features enabled to make it nicer.
;; undo-tree-mode, electric-pair-mode, show-paren-mode, size-indication-mode, better settings for scrolling, etc.


;;; Code:

;; TODO:
;; Require workgroups2, in order to get the per-workgroup winner-undo feature.
;; Require desktop, in order to provide closed-buffer tracking (which relies on desktop.el), even though by default it won't be saved (make sure desktop not set to save or load by default).
;; replace paredit by smartparens

(require 'whitespace)
(require 'face-remap)
(require 'savehist)
(require 'desktop)
(require 'message)
(require 'eldoc)
(require 'ivy)
(require 'counsel)
(require 'hydra)
(require 'dired-x) ; For dired-jump
(load "cl-seq") ; For the CL «position» and «intersection»
(eval-and-compile
  (require 'cl) ; Load-time too, for ⌜position⌝ and ⌜intersection⌝ aliases
  (require 'vimizer)) ; For the «silently» macro, and global-set-key-list
(require 'workgroups)  ; TODO: Switch to workgroups2, but turn off its save/restore so it doesn't conflict with desktop mode.
(require 'paredit) ; TODO: maybe smartparens instead
(require 'expand-region) ; TODO: maybe something else
(require 'highlight-symbol) ; TODO: probably not
(require 'usablizer)
(require 'scad)
(require 'sunrise-commander)
(require 'sunrise-x-checkpoints)
(require 'sunrise-x-loop)
(require 'sunrise-x-mirror)
(require 'sunrise-x-modeline)
(require 'sunrise-x-popviewer)
(require 'sunrise-x-tabs)
(require 'sunrise-x-tree)
(require 'org)
(require 'mu4e)

;; Silence byte compiler
(declare-function 'position "cl")
(declare-function 'intersection "cl")
;; ...but it whines about them anyway. Yay, Emacs.


;;; Utilities

(defmacro mv-to-head (elt list)
  "Move ELT to head of LIST. Add if not already present.
Delete all occurrences of ELT from LIST, then push ELT to head of LIST."
  (let ((oelt (gensym)))
    `(let ((,oelt ,elt)) ; For the push, in case elt is a function of list
       (setq ,list (delq ,oelt ,list))
       (push ,oelt ,list))))

(defun strikethrough-compatible-font-p ()
  "Return t if the default font is known to render U0336 as strikethrough composed with preceding character."
  (let ((font (face-attribute 'default :family)))
    (or (string= font "DejaVu Sans")
	(string= font "DejaVu Serif"))))

;; Uses wg-insert-elt, so workgroups must be loaded first. FIXME: maybe dash instead? Since workgroups2 doesn't even have wg-insert-elt
(require 'workgroups)

(defvar undo-tree-normally-off
  '(undo-tree-visualizer-mode dired-mode diff-mode calc-mode)
  "Major modes in which undo-tree is not enabled by `global-undo-tree-mode', besides those in `undo-tree-incompatible-major-modes'.")

(defvar undo-tree-off-lighter
  '(:eval (if (or undo-tree-mode undo-tree-visualizer-selection-mode
		  (memq major-mode undo-tree-normally-off)
		  (memq major-mode undo-tree-incompatible-major-modes))
	      ""
	    (if (strikethrough-compatible-font-p)
		" U̶n̶d̶o̶-̶T̶r̶e̶e̶"; If you're reading this source code using a font such as DejaVu Sans Mono that doesn't support strikethrough, use Emacs's variable-pitch-mode to see this correctly
	      " Undo-Tree-off"))) ; In case modeline font doesn't support strikethrough
  "Mode line indicator that `undo-tree-mode' is off.
In this case either linear undo is on, or undo is disabled entirely.
Less verbose than showing when undo tree is on, if you normally have it on.")

(defvar undo-tree-maybe-lighter
  '(:eval (if (or (memq major-mode undo-tree-normally-off)
		  (memq major-mode undo-tree-incompatible-major-modes))
	      (eval (car (get 'undo-tree-mode-lighter 'standard-value)))
	    ""))
  "Mode line indicator for `undo-tree-mode'. Silent in major modes in which undo-tree is normally on.")

(defvar ivy-off-lighter
  '(:eval (if ivy-mode
	      ""
	    (if (strikethrough-compatible-font-p)
		" i̶v̶y̶" ; See comments for undo-tree-off-lighter
	      " ivy-off")))
  "Mode line indicator that `ivy-mode' is off.
Less verbose than showing when it's on, if you normally have it on.")

(defvar ivy-silent-lighter
  '(:eval "")
  "Mode line indicator for `ivy-mode'.
Silent.")

;; TODO: Getting way too long; invert, and say only text-mode and prog-mode normally have whitespace-mode on.
(defvar whitespace-mode-normally-off
  '(help-mode term-mode shell-mode eshell-mode undo-tree-visualizer-mode diff-mode grep-mode occur-mode debugger-mode messages-buffer-mode package-menu-mode Info-mode Man-mode woman-mode ibuffer-mode calc-mode apropos-mode inferior-emacs-lisp-mode completion-list-mode dired-mode)
  "Major modes in which whitespace mode is not enabled by `global-whitespace-mode'.")

(defvar whitespace-mode-off-lighter
  '(:eval (if (or whitespace-mode-actually-on
		  (memq major-mode whitespace-mode-normally-off))
	      ""
	    (if (strikethrough-compatible-font-p)
		" W̶S̶" ; See comments for undo-tree-off-lighter
	      " WS-off")))
  "Mode line indicator that `whitespace-mode' is off.
Less verbose than showing when it's on, if you normally have it on.")

(defvar whitespace-mode-maybe-lighter
  '(:eval (if (memq major-mode whitespace-mode-normally-off)
	      " ws" ""))
  "Mode line indicator for `whitespace-mode'.
Silent in major modes in which whitespace-mode is normally on.")

(defvar long-lines-lighter
  '(:eval (cond (truncate-lines " NW") ; No wrap
		(word-wrap "") ; Stay silent to avoid mode line clutter in normal case
		(t " CW"))) ; Character wrap
  "Mode line indicator for the current line wrapping mode.
The mode is set by `set-line-wrap'.")

(defvar quiet-lighters '(long-lines-lighter
			 whitespace-mode-off-lighter
			 undo-tree-off-lighter
			 ivy-off-lighter))

;; Necessary to enable the lighters to work:
(put 'undo-tree-off-lighter 'risky-local-variable t)
(put 'undo-tree-maybe-lighter 'risky-local-variable t)
(put 'ivy-off-lighter 'risky-local-variable t)
(put 'ivy-silent-lighter 'risky-local-variable t)
(put 'whitespace-mode-off-lighter 'risky-local-variable t)
(put 'whitespace-mode-maybe-lighter 'risky-local-variable t)
(put 'long-lines-lighter 'risky-local-variable t)

(defun insert-after-modes (elt)
  "Insert ELT to default mode line after position of mode indicators."
  (let ((pos (position 'mode-line-modes mode-line-format)))
    (assert pos)
    (setq-default mode-line-format
		  (wg-insert-elt elt (default-value 'mode-line-format) (1+ pos)))))

(defun delete-from-mode-line (elt)
  (setq-default mode-line-format
		(delete elt (default-value 'mode-line-format))))

(defun set-minor-mode-lighter (mode lighter)
  (setcar (cdr (assoc mode minor-mode-alist)) lighter))

(defun get-minor-mode-lighter (mode)
  (car (cdr (assoc mode minor-mode-alist))))

(defun unclutter-mode-line ()
  "Show when normally-on modes are off.
Show nothing when they're on, to avoid cluttering the mode line."
  (set-minor-mode-lighter 'undo-tree-mode undo-tree-maybe-lighter)
  (set-minor-mode-lighter 'ivy-mode ivy-silent-lighter)
  (set-minor-mode-lighter 'whitespace-mode whitespace-mode-maybe-lighter)
  (set-minor-mode-lighter 'global-whitespace-mode "") ; Requires special-casing due to poor design of global-whitespace-mode in Emacs
  (mapc #'delete-from-mode-line quiet-lighters) ; Reset, in case they're already there
  (mapc #'insert-after-modes quiet-lighters) ; After, for emphasis, since normally silent
  (force-mode-line-update))

(defun clutter-mode-line ()
  "Undo `unclutter-mode-line'."
  (set-minor-mode-lighter 'undo-tree-mode
			  (eval (car (get 'undo-tree-mode-lighter 'standard-value))))
  (set-minor-mode-lighter 'ivy-mode " ivy")
  (set-minor-mode-lighter 'whitespace-mode " ws") ; Standard value
  (set-minor-mode-lighter 'global-whitespace-mode " WS")
  (mapc #'delete-from-mode-line quiet-lighters)
  (force-mode-line-update))

(defun up-down-ring-isearch ()
  "Use up and down arrow keys after isearch to select a search string from the ring. This is more convenient than pressing M-p and M-n. Enable by adding this function to `isearch-mode-hook'."
  (unless (memq this-command ; Emacs is so gay
		'(isearch-forward-exit-minibuffer isearch-reverse-exit-minibuffer))
    (define-key isearch-mode-map [down] 'isearch-ring-advance)
    (define-key isearch-mode-map [up] 'isearch-ring-retreat)
    (add-hook 'post-command-hook #'maybe-remove-up-down-ring)))

;; Search ring movement no longer needed if user is typing a search string
(defun maybe-remove-up-down-ring ()
  (if (eq this-command 'isearch-printing-char)
      (remove-up-down-ring)))

(defun remove-up-down-ring ()
  (define-key isearch-mode-map [down] nil)
  (define-key isearch-mode-map [up] nil)
  (remove-hook 'post-command-hook 'maybe-remove-up-down-ring))

;; Advice for isearch-repeat-forward
(defun isearch-repeat-forward--remove-up-down-ring ()
  "After the first time isearch is repeated, remove up and down mapping for ring movement, so that up or down will exit isearch as is standard for Emacs, instead of trying to move in the search ring, which is no longer useful after isearch has been repeated (which means the user has already found the history item he was looking for)."
  (remove-up-down-ring))

(defun isearch-repeat-backward--remove-up-down-ring ()
  "Supplement to no-up-down-ring-after-isearch-repeat"
  (remove-up-down-ring))

(defun isearch-forward-exit-minibuffer--remove-up-down-ring ()
  "Supplement to no-up-down-ring-after-isearch-repeat"
  (remove-up-down-ring))

(defun isearch-reverse-exit-minibuffer--remove-up-down-ring ()
  "Supplement to no-up-down-ring-after-isearch-repeat"
  (remove-up-down-ring))

(defvar ivy--exhibit-first-time nil)

(defun ivy--minibuffer-setup--advice (oldfun)
  "Inform `ivy--exhibit' when it's called for the first time in this minibuffer
session, so it knows to behave differently. For use in conjunction
with `ivy--exhibit--special-first-time'."
  (setq ivy--exhibit-first-time t)
  (funcall oldfun))

(defun ivy--exhibit--special-first-time (oldfun)
  "Advice to go around `ivy--exhibit' to make it behave specially the first time:
If doing `execute-extended-command', insert the last history element into the
minibuffer, so pressing enter will immediately operate on it. This enables
doing M-x [RET] to re-do the last M-x command.

Relies on `ivy--minibuffer-setup--advice'."
  (if (memq 'ivy--exhibit post-command-hook)
      (prog1
	  (funcall oldfun)
	(when ivy--exhibit-first-time
	  (setq ivy--exhibit-first-time nil)
	  (when (memq this-command '(execute-extended-command counsel-M-x))
	    (ivy-previous-history-element 1)
	    (with-current-buffer (window-buffer (minibuffer-window))
	      (move-beginning-of-line 1)
	      (set-mark (point))
	      (move-end-of-line 1)
	      (activate-mark)))))))

;; XXX: delete this; I'm no longer doing anything that causes nil to be passed.
(defun ivy--done-abort-if-nil (oldfun arg)
  "Advice to go around `ivy--done', to abort instead of passing nil, since `ivy--done' doesn't handle nil."
  (if arg
      (funcall oldfun arg)
    (minibuffer-keyboard-quit)))

;; Add this function to flyspell-mode-hook to flyspell buffer when enabling flyspell mode but not when disabling flyspell mode
;; Need this because flyspell-mode runs its hook both when turning on and when turning off
(defun maybe-flyspell-buffer ()
  (if flyspell-mode (flyspell-buffer)))

;; By default there's some overlap (file-name-history regexp-search-ring search-ring), so remove it. savehist mode dynamically grows savehist-minibuffer-history-variables, so overlaps must be dynamically removed from desktop-globals-to-save:
(defun deduplicate-savehist-desktop-vars ()
  (mapc (lambda (x) (setq desktop-globals-to-save
			  (delete x desktop-globals-to-save)))
	(intersection savehist-minibuffer-history-variables desktop-globals-to-save)))

(defun wg-quiet-update ()
  ;; wg doesn't allow any operations, not even update, while minibuffer active
  (unless (or (active-minibuffer-window) (null wg-list))
    (let ((wg-quiet t)) (wg-update-all-workgroups))))

;;Can't add auto-save hook unconditionally since desktop file might not get loaded, and can't test that condition while init file is loading because desktop-dirname isn't set until desktop-read runs, which is only in after-init-hook. Could just unconditionally add to auto-save-hook and rely on my fix for bug #14479 to remove it on first failure, but cleaner to do it this way. XXX: Or could unconditionally add a hook that removes itself if desktop-dirname is nil.
(defun conditionally-add-desktop-autosave ()
  (when desktop-dirname
    (add-hook 'auto-save-hook (lambda () (desktop-save desktop-dirname)))
    ;; Don't add again if another desktop loaded. FIXME: but I want to remove the old one, and add new one, since new one might be in different dir. Why does desktop mode separate desktop base name and dirname in the first place?
    (remove-hook 'desktop-after-read-hook 'conditionally-add-desktop-autosave)))

(with-no-warnings ; Byte compiler incorrectly claims that «i» is unused.
  ;; ...and it whines here despite the with-no-warnings. Yay, Emacs.
(defun nicizer-bind-wg-switch-keys ()
  (eval
   (let ((forms))
     `(progn
	,@(dotimes (i 10 forms)
	    (push `(defun ,(intern (format "wg-switch-silently-%d" i)) ()
		     (interactive)
		     (let ((wg-quiet t))
		       (,(intern (format "wg-switch-to-index-%d" i)))))
		  forms)
	    (push `(global-set-key ',(read (format "[M-f%d]"
						     ;; f1-f9 for 1-9; f24 for 0
						     (if (= i 0) 24 i)))
				   ',(intern (format "wg-switch-silently-%d" i)))
		  forms))))))
)

(defvar nicizer-kbd-layouts '(latin)
  "Layouts selectable by `nicizer-kbd-switch-layout'. Current layout is head of list.")

;; X on Debian 6 and 7 for some reason unloads my custom map after laptop suspend and resume, so this is to enable convenient reloading.
(defun nicizer-kbd-layout-reset ()
  "Reset keyboard layout."
  (interactive)
  (shell-command "xmodmap ~/.xmodmap-reset")
  (shell-command "xmodmap ~/.xmodmap-latin")
  (mv-to-head 'latin nicizer-kbd-layouts)
  (message "Layout reset"))

(defun nicizer-kbd-previous-layout ()
  "Switch to previous layout in `nicizer-kbd-layouts'."
  (interactive)
  (unless (cadr nicizer-kbd-layouts)
    (user-error "No other keyboard layout available"))
  (nicizer-kbd-switch-layout (cadr nicizer-kbd-layouts)))

(defun nicizer-kbd-switch-layout (layout)
  "Switch keyboard layout, and move it to head of `nicizer-kbd-layouts'.
The layout must be in the file named ⌜~/.xmodmap-φ⌝, where φ is the name of the layout."
  (interactive
   (list (intern (completing-read
		  "Keyboard layout: "
		  (mapcar #'symbol-name nicizer-kbd-layouts)
		  nil 'confirm
		  (if (cadr nicizer-kbd-layouts)
		      (symbol-name (cadr nicizer-kbd-layouts)))))))
  (let ((filename (concat "~/.xmodmap-"
			  (symbol-name layout))))
    (unless (file-readable-p filename)
      (user-error "Can't read file %s" filename))
    (shell-command (concat "xmodmap " filename))
    (message "Loaded %s" filename))
  (mv-to-head layout nicizer-kbd-layouts))

(defcustom message-outbox-directory
  (file-name-as-directory (expand-file-name "outbox" message-directory))
  "Directory where messages queued for sending are stored."
  :type 'directory
  :group 'message)

(defcustom message-queued-drafts-directory
  (file-name-as-directory (expand-file-name "queued-drafts" message-directory))
  "Directory where original drafts of messages queued for sending are stored."
  :type 'directory
  :group 'message)

;; Relies on mv-rename from http://code.activestate.com/recipes/578116-move-files-with-rename-if-required/
(defun queue-message-to-outbox ()
  (let ((filename (expand-file-name "out.mail" message-outbox-directory)))
    (delete-mail-header-separator)
    (write-file filename)
    (shell-command (concat "mv-rename " filename " " message-outbox-directory))))

(defun discard-draft ()
  ;; TODO: just do delete-file, after confirming that won't break anything here
  (basic-save-buffer)
  (shell-command (concat "mv-rename " (buffer-file-name) " " message-queued-drafts-directory)))

(defun delete-mail-header-separator ()
  "Delete `mail-header-separator'.
This function copied from top of `message-send-mail-partially' in Emacs's message.el."
  (goto-char (point-min))
  (re-search-forward
   (concat "^" (regexp-quote mail-header-separator) "\n"))
  (replace-match "\n"))

(defun nicizer-generate-random-message-id ()
  (concat "<"
	  (substring
	   (remove ?/
		   (remove ?+
			   (shell-command-to-string "head -c 48 /dev/urandom | base64")))
	   0 43)
	  "@local>"))

(defun nicizer-message-make-message-id ()
  "This function replaces the standard `message-make-message-id' in Emacs's message.el, which whines about localhost hostname."
  (nicizer-generate-random-message-id))

(defun nicizer-message-make-date-UTC (&optional now)
  "Make a valid date header.
If NOW, use that time instead.
This function copied from and identical to `message-make-date' in Emacs's message.el, except it tells `format-time-string' to use UTC."
  (let ((system-time-locale "C"))
    (format-time-string "%a, %d %b %Y %T %z" now t)))

(defun insert-random-password ()
  (interactive)
  (insert
   (substring
    (shell-command-to-string "base64 /dev/urandom | head -c 30 | sed -e 's/[^a-zA-Z0-9]//g'")
    0 20)))

(defun set-buffer-read-only ()
  (setq buffer-read-only t))

(defun add-find-file-hook-read-only (&rest args)
  (add-hook 'find-file-hook #'set-buffer-read-only))

(defun remove-find-file-hook-read-only (&rest args)
  (remove-hook 'find-file-hook #'set-buffer-read-only))

;; Copied from https://emacs.stackexchange.com/questions/24657/unadvise-a-function-remove-all-advice-from-it
(defun advice-unadvice (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

;; For adding to minibuffer-setup-hook
(defun clear-mark-ring ()
  (setq mark-ring nil))

;; For adding to post-command-hook
(defun dedup-mark-ring-head ()
  "Get rid of dups at head of mark and ring, such as left behind
by `er/expand-region' after it's cancelled."
  (while (and (mark-marker) (car mark-ring) (= (mark-marker) (car mark-ring)))
    (setq mark-ring (cdr mark-ring))))

(defun counsel-find-file-with-optional-ffap (oldfun &optional arg)
  "Advice for `counsel-find-file'. With prefix arg, tell that function to use `ffap-guesser' if the latter is successful. Fail with a message if the latter is not successful, to make that immediately obvious."
  (if (and current-prefix-arg (not (ffap-guesser)))
      (message "No file at point found")
    (let ((counsel-find-file-at-point current-prefix-arg)
	  (arg (if current-prefix-arg nil arg)))
      (funcall oldfun arg))))

;;Advice to be added to wg-mode-line-add-display
(defun work-around-incomprehensible-brokenness (oldfun &optional args)
  "Work around incomprehensible bug when calling `wg-mode-line-add-display', whereby `mode-line-format' is somehow a string property list instead of a list (regular list, satisfying listp) like it's supposed to be."
  (if (listp mode-line-format)
      (apply oldfun args)
    (message "WARNING: mode-line-format is screwed up")))

;; Advice to be added to comint-line-beginning-position
;; comint-get-old-input-default is broken, returning multiple lines when point is on old field of output text. The docs for it and comint-get-old-input say that if comint-use-prompt-regexp is nil, and point is on output field, then the current (logical) line will be sent as input (though what's really wanted is the line without the leading prompt, which is what the code actually does). But what the actual code does:
;; 1. The leading prompt (which is in output field), if there is one, is stripped off the logical line (via comint-bol), which is good, but
;; 2. The entire current field (which can be multiple logical lines) will be sent as input.
;; 3. The current field might be an output field! Reason it can be an output field is if the entire current logical line is an output field (in which case, comint-bol just does bol, since there's no input field to reach on the current logical line).
;; Solution: advise comint-line-beginning-position to be careful, and throw an error if its (documented) assumptions are violated.
(defun comint-line-beginning-position-careful (oldfun &optional arg)
  (let ((retval (funcall oldfun)))
    (if (get-char-property retval 'field)
	(user-error "No input field on current line"))
    retval))

(defun comint-insert-input-not-annoying ()
  "Do `comint-insert-input', except not annoying. Specifically, allow binding to keystroke, and don't fall back to another command if there's no input field at point."
  (interactive)
  (let ((field (field-at-pos (point)))
	(input (field-string-no-properties (point))))
    (if (or (null input) (null comint-accum-marker) field)
	(user-error "Not at input field")
      (goto-char (point-max))
      ;; First delete any old unsent input at the end
      (delete-region
       (or (marker-position comint-accum-marker)
	   (process-mark (get-buffer-process (current-buffer))))
       (point))
      ;; Insert the input at point
      (insert input))))

;; Advice for comint-send-input
(defun comint-send-or-insert-input (oldfun &rest args)
  "If point is in the input field at the end of the buffer, then do `comint-send-input'.
Otherwise, do `comint-insert-input-not-annoying', i.e. copy the input field that's under point
to the end of the buffer, where running this command again (optionally after editing the text)
will then do `comint-send-input'."
  (interactive)
  (if (= (field-end) (point-max))
      (apply oldfun args)
    (comint-insert-input-not-annoying)))

(defun overview-interactive-functions (in out)
  "Print into buffer OUT all defvars, defconsts, defmacros, and definitions of interactive functions in buffer IN, including the docstring, excluding the rest of the body."
  (let ((item t)
	(inb (get-buffer in))
	(outb (get-buffer out)))
    (unless inb
      (user-error "No input buffer supplied"))
    (unless outb
      (user-error "No output buffer supplied"))
    (while item
      (setq item (read inb))
      (when (consp item)
	    (when (or (and (equal (car item) 'defun)
			   (consp (nth 4 item))
			   (equal (car (nth 4 item)) 'interactive))
		      (equal (car item) 'defmacro))
	      (print (list (nth 0 item)
			   (nth 1 item)
			   (nth 2 item)
			   (nth 3 item)
			   '...) ; Visual reminder that the body is elided
		     outb))
	    (when (or (equal (car item) 'defvar)
		      (equal (car item) 'defvar-local)
		      (equal (car item) 'defconst))
	      (print item outb))))))


;;; Fix whitespace-mode brokenness

(defvar-local whitespace-mode-actually-on nil
  "t if whitespace mode is on.
This is unlike Emacs's `whitespace-mode' variable, which isn't set by turning on whitespace mode globally.")
(put 'whitespace-mode-actually-on 'permanent-local t) ; Necessary for accuracy because whitespace mode remains on in a buffer after changing major mode if global-whitespace-mode is on. FIXME: or does whitespace-turn-on get called again after the change of major mode, so it isn't actually necessary to have this permanent-local?

(defun whitespace-turn-on--record-actually-on ()
  (setq whitespace-mode-actually-on t))

(defun whitespace-turn-off--record-actually-off ()
  (setq whitespace-mode-actually-on nil))


;;; Enhance whitespace-mode

;; Adapted from Emacs's whitespace-space defface
(defface whitespace-space-dups
  '((((class color) (background dark))
     :background "grey20"      :foreground "darkgray")
    (((class color) (background light))
     :background "orange" :foreground "lightgray")
    (t :inverse-video t))
  "Face used to visualize SPACE DUPs."
  :group 'whitespace)

;; Adapted from Emacs's whitespace-space defvar, despite the obsolescence warning, because whitespace-color-on won't work without it. Yay, Emacs.
(defvar whitespace-space-dups 'whitespace-space-dups
  "Symbol face used to visualize SPACE DUPs.
Used when `whitespace-style' includes the value `space-dups'.")
(make-obsolete-variable 'whitespace-space-dups "use the face instead." "24.4")

;; Adapted from Emacs's whitespace-space-regexp
(defcustom whitespace-space-dups-regexp "[^\n\t ]\\( \\{2,3\\}\\)[^\n\t ]"
  "Specify SPACE DUPs regexp.

Match exactly two or three space chars between non-whitespace chars, because this both is likely a typo or copy/paste artifact and is hard to visually distinguish from a single space. Don't match four or more space chars, because that's likely intentional.

This feature would obviously be distracting for the type of people who prefer to put two spaces after their periods. The solution to that problem is for those people to strive to improve themselves by correcting their preferences. Or they could modify the regex, and continue to wallow in their waywardness...

NOTE: Enclose always by \\\\( and \\\\) the elements to highlight.
      Use exactly one pair of enclosing \\\\( and \\\\).

Used when `whitespace-style' includes `space-dups'."
  :type '(regexp :tag "SPACE DUPs")
  :group 'whitespace)

;; Following 3 setq adapted from whitespace.el, to add space-dups. (Yes, redefining constants here...)
(setq whitespace-style-value-list
  '(face
    tabs
    spaces
    space-dups
    trailing
    lines
    lines-tail
    newline
    empty
    indentation
    indentation::tab
    indentation::space
    big-indent
    space-after-tab
    space-after-tab::tab
    space-after-tab::space
    space-before-tab
    space-before-tab::tab
    space-before-tab::space
    help-newline       ; value used by `whitespace-insert-option-mark'
    tab-mark
    space-mark
    newline-mark
    ))
(setq whitespace-toggle-option-alist
  '((?f    . face)
    (?t    . tabs)
    (?s    . spaces)
    (?d    . space-dups)
    (?r    . trailing)
    (?l    . lines)
    (?L    . lines-tail)
    (?n    . newline)
    (?e    . empty)
    (?\C-i . indentation)
    (?I    . indentation::tab)
    (?i    . indentation::space)
    (?\C-t . big-indent)
    (?\C-a . space-after-tab)
    (?A    . space-after-tab::tab)
    (?a    . space-after-tab::space)
    (?\C-b . space-before-tab)
    (?B    . space-before-tab::tab)
    (?b    . space-before-tab::space)
    (?T    . tab-mark)
    (?S    . space-mark)
    (?N    . newline-mark)
    (?x    . whitespace-style)
    ))
(setq whitespace-report-list
  (list
   (cons 'empty                   whitespace-empty-at-bob-regexp)
   (cons 'empty                   whitespace-empty-at-eob-regexp)
   (cons 'trailing                whitespace-trailing-regexp)
   (cons 'space-dups              whitespace-space-dups-regexp)
   (cons 'indentation             nil)
   (cons 'indentation::tab        nil)
   (cons 'indentation::space      nil)
   (cons 'space-before-tab        whitespace-space-before-tab-regexp)
   (cons 'space-before-tab::tab   whitespace-space-before-tab-regexp)
   (cons 'space-before-tab::space whitespace-space-before-tab-regexp)
   (cons 'space-after-tab         nil)
   (cons 'space-after-tab::tab    nil)
   (cons 'space-after-tab::space  nil)
   ))

;; And have to copy the following entire function (from whitespace.el in Emacs 25.2) just to be able to add two lines of code to it. Wow, Emacs is so easy to customize! :-/

(defun whitespace-color-on ()
  "Turn on color visualization."
  (when (whitespace-style-face-p)
    ;; save current point and refontify when necessary
    (set (make-local-variable 'whitespace-point)
         (point))
    (setq whitespace-point--used
          (let ((ol (make-overlay (point) (point) nil nil t)))
            (delete-overlay ol) ol))
    (set (make-local-variable 'whitespace-font-lock-refontify)
	 0)
    (set (make-local-variable 'whitespace-bob-marker)
	 (point-min-marker))
    (set (make-local-variable 'whitespace-eob-marker)
	 (point-max-marker))
    (set (make-local-variable 'whitespace-buffer-changed)
	 nil)
    (add-hook 'post-command-hook #'whitespace-post-command-hook nil t)
    (add-hook 'before-change-functions #'whitespace-buffer-changed nil t)
    ;; Add whitespace-mode color into font lock.
    (setq
     whitespace-font-lock-keywords
     `(
       (whitespace-point--flush-used)
       ,@(when (memq 'spaces whitespace-active-style)
           ;; Show SPACEs.
           `((,whitespace-space-regexp 1 whitespace-space t)
             ;; Show HARD SPACEs.
             (,whitespace-hspace-regexp 1 whitespace-hspace t)))
       ,@(when (memq 'space-dups whitespace-active-style)
           ;; Show SPACE DUPs.
           `((,whitespace-space-dups-regexp 1 whitespace-space-dups t)))
       ,@(when (memq 'tabs whitespace-active-style)
           ;; Show TABs.
           `((,whitespace-tab-regexp 1 whitespace-tab t)))
       ,@(when (memq 'trailing whitespace-active-style)
           ;; Show trailing blanks.
           `((,#'whitespace-trailing-regexp 1 whitespace-trailing t)))
       ,@(when (or (memq 'lines      whitespace-active-style)
                   (memq 'lines-tail whitespace-active-style))
           ;; Show "long" lines.
           `((,(let ((line-column (or whitespace-line-column fill-column)))
                 (format
                  "^\\([^\t\n]\\{%s\\}\\|[^\t\n]\\{0,%s\\}\t\\)\\{%d\\}%s\\(.+\\)$"
                  whitespace-tab-width
                  (1- whitespace-tab-width)
                  (/ line-column whitespace-tab-width)
                  (let ((rem (% line-column whitespace-tab-width)))
                    (if (zerop rem)
                        ""
                      (format ".\\{%d\\}" rem)))))
              ,(if (memq 'lines whitespace-active-style)
                   0                    ; whole line
                 2)                     ; line tail
              whitespace-line prepend)))
       ,@(when (or (memq 'space-before-tab whitespace-active-style)
                   (memq 'space-before-tab::tab whitespace-active-style)
                   (memq 'space-before-tab::space whitespace-active-style))
           `((,whitespace-space-before-tab-regexp
              ,(cond
                ((memq 'space-before-tab whitespace-active-style)
                 ;; Show SPACEs before TAB (indent-tabs-mode).
                 (if whitespace-indent-tabs-mode 1 2))
                ((memq 'space-before-tab::tab whitespace-active-style)
                 1)
                ((memq 'space-before-tab::space whitespace-active-style)
                 2))
              whitespace-space-before-tab t)))
       ,@(when (or (memq 'indentation whitespace-active-style)
                   (memq 'indentation::tab whitespace-active-style)
                   (memq 'indentation::space whitespace-active-style))
           `((,(cond
                ((memq 'indentation whitespace-active-style)
                 ;; Show indentation SPACEs (indent-tabs-mode).
                 (whitespace-indentation-regexp))
                ((memq 'indentation::tab whitespace-active-style)
                 ;; Show indentation SPACEs (SPACEs).
                 (whitespace-indentation-regexp 'tab))
                ((memq 'indentation::space whitespace-active-style)
                 ;; Show indentation SPACEs (TABs).
                 (whitespace-indentation-regexp 'space)))
              1 whitespace-indentation t)))
       ,@(when (memq 'big-indent whitespace-active-style)
           ;; Show big indentation.
           `((,whitespace-big-indent-regexp 1 'whitespace-big-indent t)))
       ,@(when (memq 'empty whitespace-active-style)
           ;; Show empty lines at beginning of buffer.
           `((,#'whitespace-empty-at-bob-regexp
              1 whitespace-empty t)
             ;; Show empty lines at end of buffer.
             (,#'whitespace-empty-at-eob-regexp
              1 whitespace-empty t)))
       ,@(when (or (memq 'space-after-tab whitespace-active-style)
                   (memq 'space-after-tab::tab whitespace-active-style)
                   (memq 'space-after-tab::space whitespace-active-style))
           `((,(cond
                ((memq 'space-after-tab whitespace-active-style)
                 ;; Show SPACEs after TAB (indent-tabs-mode).
                 (whitespace-space-after-tab-regexp))
                ((memq 'space-after-tab::tab whitespace-active-style)
                 ;; Show SPACEs after TAB (SPACEs).
                 (whitespace-space-after-tab-regexp 'tab))
                ((memq 'space-after-tab::space whitespace-active-style)
                 ;; Show SPACEs after TAB (TABs).
                 (whitespace-space-after-tab-regexp 'space)))
              1 whitespace-space-after-tab t)))))
    (font-lock-add-keywords nil whitespace-font-lock-keywords t)
    (font-lock-flush)))


;;; Monospace mode

;; Derived from buffer-face-mode-face in face-remap.el
;; Set to '(:family "DejaVu Sans Mono") to force DejaVu
(defcustom monospace-mode-face 'fixed-pitch
  "The face specification used by `monospace-mode'.
It may contain any value suitable for a `face' text property,
including a face name, a list of face names, a face-attribute
plist, etc."
  :type '(choice (face)
		 (repeat :tag "List of faces" face)
		 (plist :tag "Face property list"))
  :group 'display)

;; Derived from variable-pitch-mode in face-remap.el, but fixed to behave like buffer-face-mode (as the variable-pitch-mode docstring in Emacs 24.4 claims), i.e. to not toggle by default when called non-interactively (buffer-face-mode, like all standard minor modes, doesn't). This fix is necessary so that the function will enable rather than toggle when added to major mode hooks.
(defun monospace-mode (&optional arg)
  "Monospace default-face mode.
An interface to `buffer-face-mode' that uses `monospace-mode-face'.
Besides the choice of face, it is the same as `buffer-face-mode', but quiet."
  (interactive (list (or current-prefix-arg 'toggle)))
  (if (eq arg 'toggle)
      (buffer-face-toggle monospace-mode-face)
    (buffer-face-set (and (> (prefix-numeric-value arg) 0)
			  monospace-mode-face))))

(defvar buffer-face-mode-lighter " BufFace")

;; Add this function to buffer-face-mode-hook to get the right lighter for monospace mode
(defun set-monospace-mode-lighter ()
  (setq-local buffer-face-mode-lighter
	      (if (equal buffer-face-mode-face monospace-mode-face)
		  " Mono"
		" BufFace")))


;;; Text-Browse minor mode

;; Emacs's view-mode implements a bunch of keys that I neither need nor want, and doesn't implement some that I do (e.g. up and down arrows scroll the window rather than move the cursor), and doesn't hide the cursor, or enable word-wrap. Text-Browse minor mode solves this.
(defvar-local tbmm-original-read-only nil)
(defvar-local tbmm-original-line-wrap nil)

(defvar text-browse-minor-mode-map (make-keymap))
(mapc (lambda (x) (define-key text-browse-minor-mode-map (car x) (cadr x)))
      '(([remap previous-line] scroll-down-line) ; Scroll content down, not scroll view down
	([remap previous-logical-line] scroll-down-line)
	([remap next-line] scroll-up-line)
	([remap next-logical-line] scroll-up-line)
	([remap forward-to-word] scroll-up-command)
	(" " scroll-up-command)
	([backspace] scroll-down-command)
	([delete] scroll-down-command)
	([home] silent-beginning-of-buffer)
	([end] silent-end-of-buffer)))

(define-minor-mode text-browse-minor-mode
  "Browse text, rather than editing it.
Uses web-browser-style keybindings."
  nil " Text-Browse" 'text-browse-minor-mode-map
  (if text-browse-minor-mode
      (progn
	(setq tbmm-original-read-only buffer-read-only)
	(setq buffer-read-only t)
	(setq tbmm-original-line-wrap (get-line-wrap))
	(set-line-wrap 'word)
	(suppress-show-paren-mode 'text-browse-minor-mode)
	(suppress-dynamic-cursor-mode 'text-browse-minor-mode)
	(suppress-cursor-type 'text-browse-minor-mode))
    (progn
      (setq buffer-read-only tbmm-original-read-only)
      (set-line-wrap tbmm-original-line-wrap)
      (unsuppress-show-paren-mode 'text-browse-minor-mode)
      (unsuppress-cursor-type 'text-browse-minor-mode)
      (unsuppress-dynamic-cursor-mode 'text-browse-minor-mode))))

(defun text-browse-minor-mode-toggle ()
  "Toggle `text-browse-minor-mode' without superfluous message'." ; Mode is already indicated on the mode line
  (interactive)
  (text-browse-minor-mode (if text-browse-minor-mode 0 t)))


;;; Dynamic-Cursor minor mode

(define-minor-mode dynamic-cursor-mode
  ;; Docstring derived from Transient Mark mode in simple.el.
  "Toggle Dynamic Cursor mode.
With a prefix argument ARG, enable Dynamic Cursor mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
Dynamic Cursor mode if ARG is omitted or nil.

Dynamic Cursor mode is a global minor mode.  When enabled,
`cursor-type' is set dynamically to reflect `mark-active'.

To set `cursor-type' manually or using another mode, first disable
Dynamic Cursor mode.

Dynamic Cursor mode can be enabled or disabled buffer-locally
using (setq-local dynamic-cursor-mode t)
or (setq-local dynamic-cursor-mode nil).
This will override the global setting."
  :global t)

(defun dcm--deactivate-mark ()
  (if dynamic-cursor-mode (setq cursor-type t)))

(defun dcm--activate-mark ()
  (if dynamic-cursor-mode (setq cursor-type 'bar)))

;; Adding these hooks permanently, rather than adding/removing them in DCM's on/off function, enables DCM to work properly even when turned on/off buffer-locally.
(add-hook 'deactivate-mark-hook #'dcm--deactivate-mark)
(add-hook 'activate-mark-hook #'dcm--activate-mark)


;;; Simple manual stopwatch.

(defvar nicizer-stopwatch-mark nil)

(defun nicizer-reset-stopwatch ()
  (interactive)
  (setq nicizer-stopwatch-mark (current-time)))

(defun nicizer-read-stopwatch ()
"Echo time since stopwatch was reset."
  (interactive)
  (message "%s" (float-time (time-since nicizer-stopwatch-mark))))


;;; Typing speed recorder, with adjustment for corrected typos
;;; Duration printed is time spent typing. Divide by duty cycle to get total length of session.

(defvar current-typing-burst-start-time nil)
(defvar current-typing-burst-end-time nil)
(defvar current-typing-burst-char-count nil)
(defvar current-typing-burst-typo-count nil)
(defvar typing-burst-history nil
  "List of typing bursts.
 Each element is (start-time char-count typo-count duration).")

(defun init-typing-burst (currtime)
  (setq current-typing-burst-start-time currtime)
  (setq current-typing-burst-end-time currtime)
  (setq current-typing-burst-char-count 1)
  (setq current-typing-burst-typo-count 0))

(defun dump-typing-burst-history ()
  (if typing-burst-history
      (let (start-time ; typing-burst-history is in reverse chronological order
	    (end-time (+ (float-time (caar typing-burst-history)) (cadddr (car typing-burst-history))))
	    (char-count 0) (typo-count 0) (duration 0))
	(mapc (lambda (x)
		  (setq start-time (car x))
		  (incf char-count (cadr x))
		  (incf typo-count (caddr x))
		  (incf duration (cadddr x)))
		typing-burst-history)
	(append-to-file (format "%s, %d words, %d typos, %dh %.2fm, %.2fWPM, accuracy %.2f%%, duty %.2f%%\n"
				(format-time-string "%Y %b %d %H:%M:%S%Z" start-time)
				(/ (- char-count typo-count) 6); 6 chars/word in English, including spaces (official standard is 5 chars/word, including spaces, but the standard is a lie; the truth is closer to 5 chars/word, excluding spaces). Typo adjustment is approximate, since a sequence of delete-backward-char and backward-delete-word commands can delete both typoed chars and nearby correct chars. Total chars that remain inserted into buffer by self-insert-command can be less than (- char-count typo-count), both because backward-delete-word can delete multiple chars but counts as only one typo, and because no deletion commands count as typo corrections unless done within a typing burst.
				typo-count
				(floor duration 3600) (/ (mod duration 3600) 60)
				(* 10 (/ (- char-count typo-count) duration)) ; 60 seconds/minute, divided by 6 chars/word
				(* 100 (- 1 (/ (float typo-count) char-count)))
				(* 100 (/ duration (- end-time (float-time start-time)))))
			nil (concat user-emacs-directory "typing-session-stats"))
	(setq typing-burst-history nil))))

;; TODO: use a 1-second Emacs timer to push typing-burst-history and call init-typing-burst, and a 1-hour timer to call dump-typing-burst-history, and have typing-burst-timer and typing-burst-typo-counter just unconditionally increment their variables. But then, I can differentiate the timings for self-insert (1 second) and typo correction (5 seconds).
(defun typing-burst-timer ()
  (let ((currtime (current-time)))
    (if (not current-typing-burst-start-time) (init-typing-burst currtime)
      (let ((time-diff (float-time (time-subtract currtime current-typing-burst-end-time))))
	(if (> time-diff 3600) (dump-typing-burst-history))
	(if (< time-diff 1.0) ; Burst still in progress (not dropped below 10wpm)
	    (progn
	      (setq current-typing-burst-end-time currtime)
	      (incf current-typing-burst-char-count))
	  (if (> current-typing-burst-char-count 1) ; Burst length of one can't be timed
	      (push (list current-typing-burst-start-time current-typing-burst-char-count
			  current-typing-burst-typo-count
			  (float-time (time-subtract current-typing-burst-end-time current-typing-burst-start-time)))
		    typing-burst-history))
	  (init-typing-burst currtime))))))

(defun typing-burst-typo-counter ()
  ;; Successive backward deletions count as only one typo.
  (if (and (eq last-command 'self-insert-command) (memq this-command
		 '(delete-backward-char backward-delete-char-untabify backward-delete-word)))
      (let ((currtime (current-time)))
	(when (< (float-time (time-subtract currtime current-typing-burst-end-time)) 2)
	    (setq current-typing-burst-end-time currtime)
	    (incf current-typing-burst-typo-count)))))


;;; Structured-text mode
;;; TODO: implement this

(defvar stext-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;;FIXME: the ⌜§‡⌝ pair should go here, rather than standard-case-table, but set-case-syntax-delims modifies standard-case-table even if another table is passed, and I don't know why.
    st))

(defun stext-indent-line () (indent-relative)) ;;FIXME: indent according to structure, like Lisp mode does.

(define-derived-mode stext-mode text-mode "Stext"
  "Major mode for section-structured text."
  (set (make-local-variable 'indent-line-function) 'stext-indent-line))


;;; Miscellaneous commands

(defun zoom-standard ()
  (interactive)
  (text-scale-increase 0))

(defun zoom-in (&optional arg)
  (interactive "p")
  (text-scale-increase (or arg 1)))

(defun zoom-out (&optional arg)
  (interactive "p")
  (text-scale-decrease (or arg 1)))

;; For convenience when visually comparing to choose the better font for a display
(defun toggle-serif ()
  "Toggle display of DejaVu Sans and DejaVu Serif fonts in current buffer."
  (interactive)
  (if (string= (face-attribute 'default :family) "DejaVu Sans")
      (buffer-face-toggle '(:family "DejaVu Serif"))
    (buffer-face-toggle '(:family "DejaVu Sans"))))

(defun enable-read-write ()
  (interactive)
  (if text-browse-minor-mode (text-browse-minor-mode 0))
  (force-mode-line-update)
  (setq buffer-read-only nil))

(defun silent-push-mark-command ()
  ;; "Mark set" message is superfluous, since Nicizer sets cursor type with mark activation
  "Push and activate mark silently."
  (interactive)
  (push-mark-command nil t))

(defun silent-beginning-of-buffer (arg)
  "Do `beginning-of-buffer' without its message noise."
  (interactive "P")
  (silently (with-no-warnings ; Ugh
	      (beginning-of-buffer arg))))

(defun silent-end-of-buffer (arg)
  "Do `end-of-buffer' without its message noise."
  (interactive "P")
  (silently (with-no-warnings
	      (end-of-buffer arg))))

(defun switch-to-new-buffer (&optional basename)
  "Create and switch to a new buffer with a name based on BASENAME, or ⌜untitled⌝ if none given.
Enable `undo-tree-mode' and `whitespace-mode' in the new buffer, and enable auto-save."
  (interactive)
  (let ((nb (generate-new-buffer (or basename "untitled"))))
    (letrec ((gross-hack (lambda () (switch-to-buffer nb)
			   (remove-hook 'post-command-hook gross-hack))))
      (with-current-buffer nb
	(turn-on-undo-tree-mode)
	(font-lock-mode)
	(whitespace-turn-on)
	;; Don't lose buffers in crashes
	(mkdir (concat user-emacs-directory "untitled") t)
	(setq buffer-file-name
	      (format (concat user-emacs-directory "untitled/%s")
		      (buffer-name)))
	(auto-save-mode))
      ;; If running from minibuffer-based buffer switcher (e.g. Ido or Ivy), can't just switch to buffer, because aborting recursive edit overrides buffer selection on exit, and I can't just exit-recursive-edit since that'll just accept the minibuffer's default candidate and cause it to override my buffer switch, so I have to switch after aborting. This foobarness is all because I'm putting this function in ivy-mode-map so that I can press XF86Open twice to get a new buffer (once is to run switch-to-new-buffer). I originally wrote this to deal with Ido; now that I've switched to Ivy, it might have a better way to do this, but I haven't checked.
      (if (= (minibuffer-depth) 0) (switch-to-buffer nb)
	(add-hook 'post-command-hook gross-hack)
	;; FIXME: messages "Quit", and I don't think I can avoid it without changing the C code.
	(abort-recursive-edit)))))

;; Jump to line of current file in dired.
(defun dired-jump-from-ivy ()
  "`dired-jump', and escape minibuffer.
See comments in code for `switch-to-new-buffer' for details."
  (interactive)
  (letrec ((gross-hack-dired-jump
	    (lambda () (dired-jump)
	      (remove-hook 'post-command-hook gross-hack-dired-jump))))
    (add-hook 'post-command-hook gross-hack-dired-jump)
    (abort-recursive-edit)))

(defun conlock ()
  "Lock the console using vlock."
  (interactive)
  (shell-command "vlock -ans"))

(defun copy-last-message ()
  "Copy to `clip-ring' the last message printed to echo area."
  (interactive)
  (with-current-buffer "*Messages*"
    (goto-char (point-max))
    (line-select-minor-mode)
    (not-presumptuous-copy-region (mark) (point))
    (deactivate-mark)))

;; FIXME: how to implement this?
;; (defun ivy-switch-buffer-other-window-action-interactive ()
;;   (interactive)
;;   (ivy--switch-buffer-other-window-action ""))

;; Can't just use `ivy-partial', since that fails to do `ivy--cd' and maybe other things too. Setting this-command to ivy-partial-or-done works so far as I've tested it (needed because other code assumes it). However, there's an additional code path in ivy-partial-or-done (which is Ivy's standard binding for [tab]) that I'm not sure how to test. Therefore, to be safe, I'm avoiding this simple definition of ivy-partial-not-annoying, and using the more complex one below.
;; (defun ivy-partial-not-annoying ()
;;   "Do interactive `ivy-partial' correctly."
;;   (interactive)
;;   (setq this-command 'ivy-partial-or-done)
;;   (ivy-partial))

(defun ivy-partial-not-annoying ()
  "Do interactive `ivy-partial' correctly."
  (interactive)
  (setq this-command 'ivy-partial-or-done)
  (cl-letf (((symbol-function 'ivy-done) (lambda () ())))
    (ivy-partial-or-done)))

(defun sr-dired-do-copy-not-annoying (&optional arg)
  "Do `dired-do-copy' within Sunrise Commander without the latter setting
the default target directory to that of the opposite pane, since that's the appropriate
behavior for `sr-do-copy' but not for `dired-do-copy'.

See also `sr-dired-do-rename-not-annoying'."
  (interactive "P")
  (setq this-command 'dired-do-copy)
  (ad-deactivate 'dired-dwim-target-directory)
  (let ((retval (call-interactively 'dired-do-copy arg)))
    (ad-activate 'dired-dwim-target-directory)
    retval))

(defun sr-dired-do-rename-not-annoying (&optional arg)
  "Do `dired-do-rename' within Sunrise Commander without the latter setting
the default target directory to that of the opposite pane, since that's the appropriate
behavior for `sr-do-rename' but not for `dired-do-rename'.

See also `sr-dired-do-copy-not-annoying'."
  (interactive "P")
  (setq this-command 'dired-do-rename)
  (ad-deactivate 'dired-dwim-target-directory)
  (let ((retval (call-interactively 'dired-do-rename arg)))
    (ad-activate 'dired-dwim-target-directory)
    retval))

(defun paredit-backward-delete-word ()
  "Delete a word backward, skipping over any intervening delimiters."
  (interactive)
  (cl-letf (((symbol-function 'backward-kill-word) #'backward-delete-word))
    (paredit-backward-kill-word)))


;;; Init

;;;###autoload
(defun nicizer-bind-keys ()
  "Hijack the user's keybindings."
  (interactive)
  (global-set-key-list
   `(

     ;; Some paredit commands useful even when not in paredit mode
     ([f14] paredit-backward-slurp-sexp)
     ([S-f14] paredit-backward-barf-sexp)
     ([M-f14] paredit-join-sexps)
     ([M-S-f14] paredit-split-sexp)
     ([f17] paredit-forward-slurp-sexp)
     ([S-f17] paredit-forward-barf-sexp)
     ([M-f17] paredit-raise-sexp)
     ([M-S-f17] paredit-splice-sexp)
     (,(kbd "M-(") paredit-wrap-round)
     (,(kbd "M-[") paredit-wrap-square)

     ;; Miscellaneous
     ([M-menu] text-browse-minor-mode-toggle)
     ([S-menu] nicizer-kbd-previous-layout)
     ([M-S-menu] nicizer-kbd-switch-layout)
     ([S-f10] nicizer-kbd-layout-reset)
     ([M-XF86Save] UNUSED) ; TODO: display versions of current file
     ([M-S-XF86Save] UNUSED) ; TODO: display versions of file at point
     ([M-S-XF86Save] enable-read-write)
     ([S-XF86Forward] monospace-mode)
     ([M-XF86Forward] UNUSED)
     ([M-S-XF86Forward] zoom-in)
     ([M-XF86Back] zoom-standard)
     ([M-S-XF86Back] zoom-out)
     ([f16] er/expand-region)
     ([s-S-XF86Open] wg-switch-to-previous-workgroup)
     (,(kbd "s-U") wg-switch-to-previous-workgroup)
     ([s-cancel] conlock)
     ([M-S-delete] copy-last-message)
     ([find] swiper)
     ([S-find] UNUSED) ; TODO: search backward
     ([M-find] multi-occur-in-matching-buffers)
     ([M-S-find] sunrise-cd)
     ([XF86Calculator] calc)
     ([S-XF86Calculator] mu4e)
     ([M-S-XF86Calculator] rgrep)
     ([XF86Search] nicizer-toggle-web-window) ; For browsing read-only docs: web, info, help, etc
     ([S-XF86Search] nicizer-search-web) ; Local desktop and web archive search; remote with uarg
     ([M-XF86Search] UNUSED)
     ([M-S-XF86Search] UNUSED)
     ([f6] goto-line)
     ([f7] insert-random-password)
     ([f8] nicizer-read-stopwatch)
     ([f9] nicizer-reset-stopwatch)
     ([S-XF86Open] counsel-find-file)
     ([C-menu] counsel-M-x)
     ([M-help] counsel-unicode-char)
     (,(kbd "C-c C-r") ivy-resume)

     ;; Replace Vimizer and Usablizer bindings to reduce message noise
     ([SunFront] silent-push-mark-command)
     ([M-home] silent-beginning-of-buffer)
     ([M-end] silent-end-of-buffer)))

  (nicizer-bind-wg-switch-keys)

  ;; Most of paredit's bindings are annoying, so get rid of them.
  ;; Patterned on paredit's paredit-define-keys function.
  (paredit-do-commands (spec keys fn examples)
      nil
    (dolist (key keys)
      (define-key paredit-mode-map (read-kbd-macro key) nil)))

  ;; But keep the plain char ones
  (mapc (lambda (x) (define-key paredit-mode-map (read-kbd-macro (car x)) (cadr x)))
	'(("(" paredit-open-round)
	  (")" paredit-close-round)
	  ("[" paredit-open-square)
	  ("]" paredit-close-square)
	  ("\"" paredit-doublequote)
	  ("\\" paredit-backslash)
	  (";" paredit-semicolon)))
  ;; And the following needed since plain backward-delete-word would screw up paredit mode
  (define-key paredit-mode-map [S-backspace] 'paredit-backward-delete-word)

  (define-key sr-mode-map [XF86Back] 'sr-history-prev)
  (define-key sr-mode-map [XF86Forward] 'sr-history-next)
  (define-key sr-mode-map [S-home] 'sr-beginning-of-buffer)
  (define-key sr-mode-map [S-end] 'sr-end-of-buffer)
  (define-key sr-mode-map "D" 'dired-do-delete) ; Disregard the unnecessary sr-do-delete
  (define-key sr-mode-map "x" 'dired-do-flagged-delete) ; Disregard the superfluous sr-do-flagged-delete
  (define-key sr-mode-map "\M-C" 'sr-dired-do-copy-not-annoying)
  (define-key sr-mode-map "\M-R" 'sr-dired-do-rename-not-annoying)

  (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)
  (define-key ivy-minibuffer-map [return] 'ivy-alt-done) ; Default is 'ivy-done, which opens candidate dir in dired, which is annoying, instead of just completing the candidate.
  (define-key ivy-minibuffer-map [S-return] 'ivy-immediate-done)
  (define-key ivy-minibuffer-map [M-return] 'ivy-call)
  (define-key ivy-minibuffer-map [tab] 'ivy-partial-not-annoying)
  (define-key ivy-minibuffer-map [M-home] 'ivy-beginning-of-buffer)
  (define-key ivy-minibuffer-map [M-end] 'ivy-end-of-buffer)
  (define-key ivy-minibuffer-map [M-up] 'ivy-previous-history-element)
  (define-key ivy-minibuffer-map [M-down] 'ivy-next-history-element)
  (define-key ivy-minibuffer-map [find] 'ivy-next-line-or-history)
  (define-key ivy-minibuffer-map [S-find] 'ivy-reverse-i-search)
  (define-key ivy-minibuffer-map [remap backward-delete-word] 'ivy-backward-kill-word)

  ;; Get rid of mu4e's infuriating bindings
  (define-key mu4e-view-mode-map (kbd "<M-down>") nil)
  (define-key mu4e-view-mode-map (kbd "<M-up>") nil)
  (define-key mu4e-view-mode-map (kbd "<home>") nil)
  (define-key mu4e-view-mode-map (kbd "<end>") nil))

;;;###autoload
(defun nicizer-init ()
  "Hijack the user's settings.
Add hooks to set bar cursor when mark is active, and block cursor when mark is inactive.
Show whitespace, set variable-pitch font, unclutter the mode line.
Many others."
  (interactive)

  ;; Add delimiter pairs missing from Emacs
  ;; For electric-pair-mode, show-paren-mode, and sexp-based movement/editing commands
  (mapc (lambda (x)
	  (set-case-syntax-delims (car x) (cadr x) (standard-case-table)))
	'((?‹ ?›)
	  (?❬ ?❭)
	  (?❮ ?❯)
	  (?« ?») ; This was removed from Emacs 24.4 in deference to Germans, who are backward
	  (?“ ?”)
	  (?⌜ ?⌝) ; Quasi-quotation marks
	  (?⌞ ?⌟)
	  (?⎡ ?⎤) ; Integer ceiling
	  (?⎣ ?⎦))) ; Floor

  ;; Improved settings
  (setq inhibit-startup-screen t)
  (setq initial-scratch-message nil)
  (fset 'display-startup-echo-area-message (lambda nil nil)) ; (setq inhibit-startup-echo-area-message t) doesn't suffice, due to the “weirdly extreme treatment” described for it in startup.el
  (fset 'yes-or-no-p 'y-or-n-p)
  (setq confirm-kill-emacs 'y-or-n-p)
  (setq undo-tree-visualizer-diff t) ; FIXME: not persisting, apparently bug in undo-tree, despite var being set buffer-local before being set to nil
  (maybe-set 'mark-ring-max 32) ; With both reverse and forward rotation provided in Usablizer, a large ring is wieldy. But don't override user's custom config.
  (maybe-set 'diff-switches "-u")
  (setq use-empty-active-region t)
  (setq save-interprogram-paste-before-kill t)
  (setq switch-to-buffer-preserve-window-point t)
  (setq scroll-preserve-screen-position t)
  (maybe-set 'scroll-conservatively 2)
  (setq scroll-error-top-bottom t)
  (setq isearch-allow-scroll t)
  (setq-default word-wrap t)
  (size-indication-mode)
  (blink-cursor-mode 0)
  (dynamic-cursor-mode)
  (column-number-mode)
  (electric-pair-mode)
  (show-paren-mode)
  (menu-bar-mode 0) ; Permanent menu bar is pointless; use menu-bar-open
  (tool-bar-mode 0)
  (winner-mode)
  (put 'narrow-to-region 'disabled nil)
  (unclutter-mode-line) ; To undo this, use: (clutter-mode-line)
  (setq dired-omit-verbose nil) ; Avoid noise in Sunrise Commander
  (setq dired-omit-files "^\\.$\\|^\\.\\.$") ; Only omit dot and dot-dot
  (setq dired-omit-extensions nil)
  (customize-set-variable 'sr-use-commander-keys nil) ; Disable redundant keybindings (which also conflict with Usablizer's use of F2 and F3 for uarg-2 and uarg-3). Must use customize-set-variable instead of just setq because of the variable's use of a custom setter.
  (setq dired-listing-switches "-alD") ; More reliable dired
  (setq sr-listing-switches "-alD")
  (setq sr-traditional-other-window t)
  (setq windmove-wrap-around t)

  ;; Parameterize to enable setting lighter buffer-locally
  (setcar (cdr (assoc 'buffer-face-mode minor-mode-alist))
	  'buffer-face-mode-lighter)

  ;; From whattheemacsd.com
  (put 'paredit-forward-delete 'delete-selection 'supersede)
  (put 'paredit-backward-delete 'delete-selection 'supersede)
  (put 'paredit-open-round 'delete-selection t)
  (put 'paredit-open-square 'delete-selection t)
  (put 'paredit-doublequote 'delete-selection t)
  (put 'paredit-newline 'delete-selection t)

  (eldoc-add-command
   'paredit-backward-delete
   'paredit-backward-delete-word
   'paredit-close-round
   ;; I'm including below only my commands not covered by the completions included by the call to eldoc-add-command-completions at the end of eldoc.el:
   'uf-backward-sexp
   'uf-forward-sexp
   'not-weird-beginning-of-defun
   'not-weird-end-of-defun
   'out-list
   'in-list
   'home-list
   'end-list)

;; Highlight erroneous whitespace
  ;; But if user already customized, then don't override that or turn on globally
  (if (maybe-set 'whitespace-style
		 '(face empty trailing space-before-tab::tab space-after-tab::tab space-dups))
      (global-whitespace-mode))

  (setq whitespace-global-modes '(not dired-mode sr-mode))

  ;; Use variable-pitch font by default
  (when (string= (face-attribute 'default :family) "DejaVu Sans Mono") ; Don't override user's custom config
    (set-face-attribute 'default nil :family "Sans Serif") ; Or "DejaVu Serif"
    (mapc (lambda (x) (add-hook x #'monospace-mode))
	  '(term-mode-hook
	    dired-after-readin-hook ; Because dired-mode uses spaces for column alignment, instead of using tabulated-list-mode
	    ibuffer-mode-hook ; Ditto
	    shell-mode-hook ; Because many commands' output formats, e.g. from ⌜ls -l⌝, have the same problem that dired-mode has
	    eshell-mode-hook ; Ditto
	    prog-mode-hook))) ; Sop to the luddites

  (add-hook 'buffer-face-mode-hook #'set-monospace-mode-lighter)
  (add-hook 'rotate-mark-ring-hook #'track-mark-ring-position) ; See Usablizer
  (add-hook 'minibuffer-setup-hook 'clear-mark-ring)
  (add-hook 'post-command-hook #'dedup-mark-ring-head)
  (add-hook 'flyspell-mode-hook #'maybe-flyspell-buffer)

;; Not using, since only works with execute-extended-command, not counsel-M-x.
;; (advice-add #'ivy--minibuffer-setup :around #'ivy--minibuffer-setup--advice)
;; (advice-add #'ivy--exhibit :around #'ivy--exhibit--special-first-time)

  (advice-add #'whitespace-turn-on :after #'whitespace-turn-on--record-actually-on)
  (advice-add #'whitespace-turn-off :after #'whitespace-turn-off--record-actually-off)

  ;; Better access to the search history ring
  (advice-add 'isearch-repeat-forward :after
	      #'isearch-repeat-forward--remove-up-down-ring)
  (advice-add 'isearch-repeat-backward :after
	      #'isearch-repeat-backward--remove-up-down-ring)
  (advice-add 'isearch-forward-exit-minibuffer :before
	      #'isearch-forward-exit-minibuffer--remove-up-down-ring)
  (advice-add 'isearch-reverse-exit-minibuffer :before
	      #'isearch-forward-exit-minibuffer--remove-up-down-ring)
  (add-hook 'isearch-mode-hook #'up-down-ring-isearch)
  ;; To prevent maybe-remove-up-down-ring from being left in post-command-hook
  (add-hook 'isearch-mode-end-hook #'remove-up-down-ring)

  (advice-add 'counsel-find-file :around 'counsel-find-file-with-optional-ffap)

  ;; Bugfix
  (advice-add 'comint-line-beginning-position :around
	      'comint-line-beginning-position-careful)

  (advice-add 'comint-send-input :around
	      'comint-send-or-insert-input))

;;;###autoload
(defun nicizer-init-niche ()
  "Settings that you probably don't want."
  (remove-hook 'prog-mode-hook 'monospace-mode) ; Remove the sop to the luddites
  (add-find-file-hook-read-only) ; Avoid accidentally modifying stuff

  (advice-add 'package-generate-autoloads :before ; autoload-generate-file-autoloads barfs if find-file is hooked to set buffer read-only
	      #'remove-find-file-hook-read-only)
  (advice-add 'package-generate-autoloads :after ; restore read-only hook to find-file after autoload-generate-file-autoloads is done
	      #'add-find-file-hook-read-only)
  (advice-add 'mu4e~draft-open-file :before
	      #'remove-find-file-hook-read-only) ;avoid more barfage
  (advice-add 'mu4e~draft-open-file :after
	      #'add-find-file-hook-read-only) ; and restore after done

  (add-hook 'message-mode-hook (lambda () (auto-fill-mode 0))) ; FIXME: insert (at front) hook to message-send-mail-function to check for lines longer than 1000 chars, and offer to do hard word wrap before sending. And test this.
  (setq debug-on-error t)
  (setq eval-expression-print-length nil)
  (setq eval-expression-print-level nil)
  (setq undo-tree-visualizer-timestamps nil) ; Timestamps are useless since my patch was rejected
  ;; (setq undo-tree-visualizer-relative-timestamps nil)
  (setq undo-tree-enable-undo-in-region nil)
  (setq backward-delete-char-untabify-method nil) ; Emacs's default (untabify) is annoying
  (setq x-stretch-cursor t) ; When on a tab char, it's best to notice that fact
  (setq mouse-autoselect-window t)
  (set-face-attribute 'default nil :height 98) ; Emacs default is 98 (9.8 points according to the docstring, though the math shows that Emacs must be assuming about 0.23 dot pitch (110 ppi)), which has monospace char size 8×15 pixels (including line and character spacing), proportional size 3-11×15 (width 3 for i and j, 11 for W). This is the smallest that clearly renders curly quotes and the curvature of a comma in proportional font DejaVu Sans Serif. On 1600×1200 15-inch T60p laptop display (about 0.19mm dot pitch; 133 ppi), this is good at about 65cm viewing distance (about one arc-minute). For coarser displays, keep 98 size (which keeps pixel size of chars) and increase viewing distance to maintain angular resolution (93cm for 1920×1200 24-inch, and 87cm for 2560×1600 30-inch). To take advantage of finer displays, keep 98 size decrease viewing distance (e.g. 58cm for 1920×1200 15-inch T61p laptop).
  ;; Smaller increments are:
  ;; 90, which has monospace char size 7×15 pixels, proportional size 3-11×15 (width 3 for i and j, 11 for W). (That was measured on 1400×1050 15-inch display; on 1600×1200 15-inch, height is 14 pixels, not 15. Maybe I mis-measured.)
  ;; 83, which has monospace char size 7×14 pixels, proportional size 3-9×15 (width 3 for i and j, 9 for W).
  ;; 75, which is still readable but too small for comfort.
  (set-scroll-bar-mode nil) ; TODO: have (thin) scroll bars, but overlay them with the gutters, rather than put them parallel to the gutters. This avoids wasting space. Use just a lightweight-shaded block for the scroll bar's slider to avoid obscuring the markings shown in the gutters.
  ;; (add-hook 'emacs-lisp-mode-hook #'turn-on-eldoc-mode)
  ;; (add-hook 'lisp-interaction-mode-hook #'turn-on-eldoc-mode)
  ;; (add-hook 'ielm-mode-hook #'turn-on-eldoc-mode)
  ;; (set-case-syntax-delims ?§ ?‡ (standard-case-table)) ; FIXME: choose something else
  (mkdir (concat user-emacs-directory "backupfiles") t)
  (mkdir (concat user-emacs-directory "autosavefiles") t)
  (setq backup-directory-alist
	`(("." . ,(concat user-emacs-directory "backupfiles"))))
  (setq backup-by-copying t)
  (setq auto-save-file-name-transforms
	`((".*/\\(.*\\)" ,(concat user-emacs-directory "autosavefiles/\\1") t)))
  (setq auto-save-include-big-deletions t)
  (setq auto-save-default t) ; t is currently the Emacs default, but let's make sure
  (setq ispell-personal-dictionary (concat user-emacs-directory "ispell_personal"))
  (setq tags-file-name nil)
  (setq tags-table-list '("/usr/local/src/emacs/TAGS" "/usr/local/src/emacs/TAGS-LISP"))

  (ivy-mode t)
  ;; The following enables pressing XF86Open twice to create and switch to a new buffer, and S-XF86Open twice to do dired-jump. (Usablizer's related global keybindings enable pressing XF86Open once to open buffer switcher, or S-XF86Open once to open file finder.)
  (define-key ivy-minibuffer-map '[XF86Open] 'switch-to-new-buffer)
  (define-key ivy-minibuffer-map '[S-XF86Open] 'dired-jump-from-ivy)

  (eval-and-compile (require 'bookmark+)) ; TODO: Review
  (eval-after-load "info" '(require 'info+)) ; For Info-merge-subnodes
  (setq bookmark-default-file (concat user-emacs-directory "bookmarks"))
  (setq bookmark-save-flag nil)

  ;; TODO: Switch to workgroups2
  (setq wg-prefix-key '[C-w])
  (workgroups-mode 1) ; TODO: workgroups2 eliminates need to use ⌜1⌝ arg to enable (rather than toggle)?
  (setq wg-morph-on nil)
  (setq wg-query-for-save-on-emacs-exit nil) ; Since I save workgroups as part of desktop instead.

  (pushnew 'kill-ring desktop-globals-to-save)
  (pushnew 'wg-list desktop-globals-to-save) ; TODO: review after upgrade to workgroups2
  (pushnew 'typing-burst-history desktop-globals-to-save)
  (pushnew 'word-wrap desktop-locals-to-save)
  ;; desktop-create-buffer in Desktop.el takes care to "Never override file system if the file really is read-only marked", but as a consequence it fails to override my enable-read-only in find-file-hook. Adding buffer-read-only to desktop-locals-to-save works around this, because desktop-create-buffer unconditionally restores it.
  (pushnew 'buffer-read-only desktop-locals-to-save)

  ;; FIXME: buffer-face-mode and text-scale-mode don't restore properly, apparently because the buffer-local vars are restored only after the modes are enabled
  ;; (pushnew 'buffer-face-mode-face desktop-locals-to-save)
  ;; (pushnew 'buffer-face-mode-lighter desktop-locals-to-save)
  ;; (pushnew 'text-scale-mode-amount desktop-locals-to-save)
  ;; (pushnew 'text-scale-mode-lighter desktop-locals-to-save)

  ;; TODO: review these after upgrade to workgroups2, and move to nicizer-init or -stateful.
  (add-hook 'desktop-save-hook #'deduplicate-savehist-desktop-vars)
  (add-hook 'desktop-save-hook #'wg-quiet-update)
  (add-hook 'desktop-after-read-hook #'conditionally-add-desktop-autosave))

;;;###autoload
(defun nicizer-init-stateless ()
  "Initialize all keybindings and features of Vimizer, Usablizer, and Nicizer."
  (interactive)
  (usablizer-bind-keys) ; Also calls vimizer-bind-keys
  (nicizer-bind-keys)
  (nicizer-init)
  (nicizer-init-niche))

;;;###autoload
(defun nicizer-init-stateful ()
  (advice-add 'wg-mode-line-add-display :around #'work-around-incomprehensible-brokenness) ; FIXME: I don't have even a clue why I have to do this, but workgroups barfs if I don't, due to mode-line-format somehow being screwed up

  "Initialize recording of Emacs state other than buffer contents."
  (setq savehist-autosave-interval 30)
  (setq desktop-base-file-name "desktop")
  (setq desktop-base-lock-name "desktop.lock")
  (if (featurep 'bookmark+) (setq bookmark-save-flag 1))
  (savehist-mode)
  (desktop-save-mode)
  (desktop-auto-save-disable) ; Disable the new feature in 24.4, since I already (conditionally) add desktop-save to auto-save-hook. The new 24.4 feature saves only when window config changes, but I also want saving when anything else changes (mark ring, kill ring, etc), and the new feature adds a new idle timer not synchronized with auto-save (of buffers) so the disk chatter is more scattered rather than consolidated. TODO: fix the new feature, then use it, and remove my custom adding of desktop-save to auto-save-hook.
  (setq desktop-restore-frames nil) ; I already save and restore workgroups, so this new feature is superfluous, and using it would mislead me, because by restoring the windows of the workgroup that was active when Emacs closed, this feature makes it appear that a workgroup has been selected upon startup even though none actually has been, which means any changes I make to the window configuration will be lost as soon as I switch to any workgroup.

  ;; Record command usage:
  (eval-and-compile (require 'keyfreq))
  (setq keyfreq-file (concat user-emacs-directory "keyfreq.data"))
  (keyfreq-mode)
  (keyfreq-autosave-mode)
  (setq keyfreq-autosave-timeout 30)

  ;; TODO: replace w/ equivalent for Ivy
  ;; (setq ido-save-directory-list-file (concat user-emacs-directory "ido.last"))

  (add-hook 'post-self-insert-hook #'typing-burst-timer)
  (add-hook 'post-command-hook #'typing-burst-typo-counter)
  (add-hook 'kill-emacs-hook #'dump-typing-burst-history)

  ;; TODO: replace w/ equivalent for Ivy
  ;; (eval-and-compile (require 'ecomplete))
  ;; (setq ecomplete-database-file (concat user-emacs-directory "ecompleterc"))
  ;; (setq message-mail-alias-type 'ecomplete)

  (pushnew 'closed-buffer-history desktop-globals-to-save)
  (pushnew 'closed-buffer-history-max-saved-items desktop-globals-to-save)
  (pushnew 'closed-buffer-history-max-full-items desktop-globals-to-save)

  ;; Outgoing message handling
  (pushnew '(utf-8 . 8bit) mm-body-charset-encoding-alist) ; Make Emacs stop mangling my messages.
  (setq message-kill-buffer-on-exit t)
  (setq message-directory "~/mail/") ; Default is "~/Mail/" (uppercase), which I don't want.
  (setq message-auto-save-directory ; Re-set because of Emacs bug #19068.
	(file-name-as-directory (expand-file-name "drafts" message-directory)))
  (setq message-outbox-directory ; Just to make sure, considering bug #19068.
	(file-name-as-directory (expand-file-name "outbox" message-directory)))
  (setq message-queued-drafts-directory ; Just to make sure, considering bug #19068.
	(file-name-as-directory (expand-file-name "queued-drafts" message-directory)))
  (mkdir message-auto-save-directory t)
  (mkdir message-outbox-directory t)
  (mkdir message-queued-drafts-directory t)
  (setq message-send-mail-function 'queue-message-to-outbox)
  (add-hook 'message-sent-hook #'discard-draft)
  (fset 'message-make-message-id 'nicizer-message-make-message-id)
  (fset 'message-make-date 'nicizer-message-make-date-UTC)

  (setq
   mu4e-sent-messages-behavior 'delete ; Because I use queue-message-to-outbox instead, then I save from there
   mu4e-maildir "/m/kd-qubes-share/kd-gandi-Maildir"
   mu4e-attachment-dir "/m/kd-qubes-share/attachments/"
   mail-user-agent 'mu4e-user-agent
   mu4e-compose-hidden-headers nil ; Get rid of some of mu4e's sneaky B.S.
   mu4e-user-agent-string nil ; And more of it
   mu4e-index-lazy-check t))


(provide 'nicizer)

;;; nicizer.el ends here
