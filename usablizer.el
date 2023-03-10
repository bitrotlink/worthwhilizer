;;; usablizer.el --- Make Emacs usable -*- lexical-binding: t; -*-
;; Version: 0.6.3
;; Package-Requires: ((emacs "25.1") (undo-tree "0.6.6") (vimizer "0.5.0"))
;; Keywords: convenience

;; This file doesn't use hard word wrap. To fold away the long comments and docstrings, use:
;; (setq truncate-lines t)
;; or with this file's functions, use:
;; (set-line-wrap 'off) ; or press s-S-right to cycle to that setting
;; To show the long lines, use:
;; (set-line-wrap 'word) ; or press s-S-right

;; For documentation, see README.org in the «worthwhilizer» Git repository


;;; Code:

(require 'misc) ; For forward-to-word
(require 'delsel) ; For minibuffer-keyboard-quit
(require 'desktop) ; Used by Usablizer's closed-buffer tracker
(require 'undo-tree) ; Emacs isn't usable without it
(require 'calc) ; So key bindings can be added
(require 'calc-aent) ; So alternate symbols can be added
(eval-and-compile (require 'vimizer)) ; For global-set-key-list, vimizer-bind-keys, and the «silently» macro
(eval-when-compile (require 'cl))

(defalias 'isearch-truncate-or-abort 'isearch-abort) ; See isearch-really-abort


;;; Utilities

;; TODO: Stefan pointed out he already has this macro in cconv.el (part of Emacs), but commented out. Delete my version after he uncomments his.
(defmacro dlet (binders &rest body)
  "Like `let', but always bind dynamically, even if `lexical-binding' is t.
Uses the local-specialness feature of `defvar'."
  (unless (listp binders) (error "%S is not a list" binders))
  ;; Contain the local-specialness, so it doesn't infect «let»s outside dlet,
  ;; because the purpose of local-specialness is to avoid global infection.
  `(progn
     ,@(let (vardefs) ; Generate all the «defvar»s
	 (dolist (binder binders (nreverse vardefs))
	   (cond ((symbolp binder)
		  (push `(defvar ,binder) vardefs))
		 ((and (listp binder)
		       (symbolp (car binder)))
		  (push `(defvar ,(car binder)) vardefs))
		 (t (error "%S is not a symbol or list" binder)))))
     (let ,binders ,@body)))

;; Add this function to find-file-hook to prevent your positions stored in registers from becoming stale when you edit a file after closing and reopening it.
(defun register-swap-back () ; This ought to be in register.el
  "Turn file-query references back into markers when a file is visited."
  (dolist (elt register-alist)
    (when (and (consp (cdr elt))
	       (eq (cadr elt) 'file-query)
	       (equal (caddr elt) buffer-file-name))
      (setcdr elt (copy-marker (cadddr elt)))
      (add-hook 'kill-buffer-hook #'register-swap-out nil t))))

;; point-to-register adds register swap outs, but it isn't used when restoring desktop, so add them by adding this function to desktop-delay-hook
(defun add-register-swap-outs ()
  (mapc (lambda (buf)
	  (catch 'done
	    ;;3 lines derived from register-swap-out in register.el
	    (dolist (elem register-alist)
	      (when (and (markerp (cdr elem))
			 (eq (marker-buffer (cdr elem)) buf))
		(with-current-buffer buf
		  (add-hook 'kill-buffer-hook #'register-swap-out nil t))
		(throw 'done nil))))) ; I don't remember where I got this idea from
	(buffer-list)))

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

(defun usablizer-fix-minibuffer-maps ()
  ;; Derived from delsel.el
  (mapc (lambda (x)
	  (define-key x [remap keyboard-quit] 'minibuffer-keyboard-quit))
	(list
	 minibuffer-local-map
	 minibuffer-local-ns-map
	 minibuffer-local-completion-map
	 minibuffer-local-must-match-map
	 minibuffer-local-isearch-map))
  (define-key minibuffer-local-isearch-map [find] 'isearch-forward-exit-minibuffer)
  (define-key minibuffer-local-isearch-map [S-find] 'isearch-reverse-exit-minibuffer))

(defun usablizer-fix-isearch-map ()
  ;; Derived from http://xahlee.org/emacs/reclaim_keybindings.html
  (mapc (lambda (x) (define-key isearch-mode-map (car x) (cadr x)))
	'(([find] isearch-repeat-forward)
	  ([S-find] isearch-repeat-backward)
	  ([XF86Paste] isearch-yank-pop)
	  ;; ([remap keyboard-quit] isearch-really-abort) ; FIXME: disabling because broke in Emacs 25.1
	  ;; ([remap partial-escape] isearch-truncate-or-abort) ; FIXME: Doesn't work, I don't know why, and don't care anymore. Hardcoding to S-escape solves the problem.
	  ([S-escape] isearch-truncate-or-abort))))

;; Change a setting only if the user hasn't already customized it
(defun maybe-set (sym val)
  "If SYM is currently set to its standard value, then set SYM to VAL and return VAL.
Otherwise, return nil."
  (if (equal (symbol-value sym)
	     (eval (car (get sym 'standard-value))))
      (set sym val)))

(defun average (&rest args)
  (/ (apply #'+ args) (length args)))

;; Copied from assq-delete-all in subr.el, but «eq» replaced by «equal»
(defun assoc-delete-all (key alist)
  "Delete from ALIST all elements whose car is `equal' to KEY.
Return the modified alist.
Elements of ALIST that are not conses are ignored."
  (while (and (consp (car alist))
	      (equal (car (car alist)) key))
    (setq alist (cdr alist)))
  (let ((tail alist) tail-cdr)
    (while (setq tail-cdr (cdr tail))
      (if (and (consp (car tail-cdr))
	       (equal (car (car tail-cdr)) key))
	  (setcdr tail (cdr tail-cdr))
	(setq tail tail-cdr))))
  alist)

(defun earmuffs-p (s)
  "Return t if string S has earmuffs (i.e. begins and ends with asterisks)."
  (let ((len (length s)))
    (and (> len 1)
	(eq ?* (string-to-char (substring s 0 1)))
	(eq ?* (string-to-char (substring s (1- len) len))))))

(defsubst usablizer-calcFunc-third (x)
  (math-normalize (list '/ x 3)))

(defsubst usablizer-calcFunc-half (x)
  (math-normalize (list '/ x 2)))

(defsubst usablizer-calcFunc-double (x)
  (math-normalize (list '* x 2)))

(defsubst usablizer-calcFunc-triple (x)
  (math-normalize (list '* x 3)))

(defun usablizer-calcFunc-cube-root (x)
  (math-nth-root x 3))

(defun usablizer-calcFunc-cube (x)
  (math-pow x 3))

(defun usablizer-calc-third (arg)
  (interactive "P")
  (calc-slow-wrapper
   (calc-unary-op "÷3" 'usablizer-calcFunc-third arg)))

(defun usablizer-calc-half (arg)
  (interactive "P")
  (calc-slow-wrapper
   (calc-unary-op "÷2" 'usablizer-calcFunc-half arg)))

(defun usablizer-calc-double (arg)
  (interactive "P")
  (calc-slow-wrapper
   (calc-unary-op "·2" 'usablizer-calcFunc-double arg)))

(defun usablizer-calc-triple (arg)
  (interactive "P")
  (calc-slow-wrapper
   (calc-unary-op "·3" 'usablizer-calcFunc-triple arg)))

(defun usablizer-calc-sqr (arg)
  (interactive "P")
  (calc-slow-wrapper
   (calc-unary-op "^2" 'calcFunc-sqr arg)))

(defun usablizer-calc-cube-root (arg)
  (interactive "P")
  (calc-slow-wrapper
   (calc-unary-op "cbrt" 'usablizer-calcFunc-cube-root arg)))

(defun usablizer-calc-cube (arg)
  (interactive "P")
  (calc-slow-wrapper
   (calc-unary-op "^3" 'usablizer-calcFunc-cube arg)))


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

(defvar-local mark-ring-position nil
  "Current position of point in the full mark ring (including mark and point).
Reset at the beginning of each sequence of consecutive calls to `reverse-rotate-mark-ring-and-point' and `rotate-mark-ring-and-point'.

Used only as state for `track-mark-ring-position'.")

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

(defun uf-backward-sexp (&optional arg)
  "Do `backward-sexp' but report more user-friendly errors."
  (interactive "p")
  (unless arg (setq arg 1))
  (condition-case nil (backward-sexp arg)
    ('scan-error (user-error (sexp-error-message arg -1)))))

(defun uf-forward-sexp (&optional arg)
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
  (if (< arg 1) (uf-forward-sexp arg) ; Backward (and zero) movement are as usual
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
  ;; XXX: Would need to do the same mad-cow dance as for forward-to-paragraph and forward-to-sexp, which would mean certainly now I need to put that dance into a macro, instead of repeating the code, but fortunately beginning-of-defun moves to next start of defun rather than to next end of defun.
  (not-weird-beginning-of-defun (- arg)))

(defun not-weird-backward-paragraph (&optional arg)
  "Like `backward-paragraph', but not weird.
Land on start of paragraph, like `backward-word' lands on start of word."
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
  "Do `find-char-inclusive' backward."
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
  "Do `find-char-exclusive' backward."
  (interactive "P")
  (setq current-prefix-arg (- (prefix-numeric-value arg)))
  (call-interactively #'find-char-exclusive))

(defun moveto-winline-1 (arg)
  "Do `move-to-window-line-top-bottom' with center as last choice."
  (interactive "P")
  (let ((recenter-positions '(top bottom middle)))
    (call-interactively #'move-to-window-line-top-bottom)))

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

(defun unindent (&optional arg)
  (interactive "P")
  (let* ((a1 (cond ((consp arg) (car arg))
		   ((integerp arg) arg)
		   (t -4)))
	 (a2 (* a1
		(if (eq (current-bidi-paragraph-direction) 'right-to-left)
		    -1 1)))
	 (beg (if (use-region-p) (region-beginning) (line-beginning-position)))
	 (end (if (use-region-p) (region-end) (line-end-position))))
    (indent-rigidly beg end a2)))

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
  "Do `keyboard-quit'."
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
  "Self explanatory."
  (interactive)
  (if (mark t)
      (if mark-active (deactivate-mark) (activate-mark))
    (user-error "No mark set in this buffer")))

(defun narrow-to-region-tweaked ()
  "Run `narrow-to-region', then deactivate mark."
  (interactive)
  (narrow-to-region (point) (mark))
  (deactivate-mark))

(defvar overlong-line-limit 950 ; Conservative limit for mail messages
  "Limit used by `goto-next-overlong-line'.")

(defun goto-next-overlong-line ()
  "Go to column `overlong-line-limit' at next line that has at least that many chars.
Return t if such a line found; otherwise, return nil and leave cursor at end of buffer."
  (interactive)
  (if (>= (current-column) overlong-line-limit) (forward-line)) ; Skip current
  (catch 'overlong
    (do ((_x nil)) ((eobp) nil)
      (move-end-of-line nil)
      (when (>= (current-column) overlong-line-limit)
	(move-beginning-of-line nil)
	(forward-char overlong-line-limit) ; Leave cursor at the limit
	(throw 'overlong t))
      (forward-line))))

(defun kill-buffer-no-ask ()
  "Do `kill-buffer'.
Bind a key to this function instead of directly to `kill-buffer' to avoid 
calling the latter interactively, so it will kill without asking."
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

;; Thanks for this technique goes to jch at:
;; https://emacs.stackexchange.com/questions/2461/how-can-i-simulate-an-arbitary-key-event-from-elisp
(defun inject-keyseq (keyseq)
  (interactive "MKey sequence: ")
  (setq unread-command-events
	(listify-key-sequence (kbd keyseq))))

(defvar window-of-last-displayed-buffer nil)

;; Advice for display-buffer
(defun save-window-of-last-displayed-buffer (x)
  (setq window-of-last-displayed-buffer x))

(defun quit-popup ()
  "Type «q» into popped-up window (typically help, occur, messages, backtrace, or undo-tree visualizer) if that will do something other than `self-insert-command'."
  (interactive)
  (unless window-of-last-displayed-buffer
    (user-error "There's no known pop-up"))
  (with-selected-window window-of-last-displayed-buffer
    (let ((binding (key-binding "q" t)))
      (if (or (null binding) (string= binding "self-insert-command"))
	  (user-error "Last displayed buffer is not quittable"))
      ;; (inject-keyseq "q") ; Nope; only happens after this command, and thus with-selected-window, already finished
      (call-interactively binding))
    (setq window-of-last-displayed-buffer nil)))

(defun dynamic-quit ()
  "Do whatever C-g is currently bound do, to deal with the Emacs ecosystem's idiotic hard-coding of the quit key."
  (interactive)
  (let ((binding (key-binding (kbd "C-g") t)))
    (setq this-command binding)
    (call-interactively binding)))

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

(defun check-parens-and-report ()
"Do `check-parens', then report if no errors are found, to avoid causing the user to wonder whether anything actually happened if no errors are found."
  (interactive)
  (unless (check-parens) (message "check-parens reported no errors")))

(defun quit-earmuffs ()
  "Do `quit-window' on all windows in the current frame showing buffers
with names satisfying `earmuffs-p', except *scratch* and *Backtrace*."
  (interactive)
  (mapc
   (lambda (w)
     (let ((bn (buffer-name (window-buffer w))))
       (and (earmuffs-p bn)
	    (not (equal bn "*scratch*"))
	    (not (equal bn "*Backtrace*"))
	    (quit-window nil w))))
   (window-list)))

(defun iconify-frame (&optional _dummy)
  "Do nothing. Override Emacs's annoying inbuilt standard function."
  (message "iconify-frame is set to do NOTHING"))

(defun move-window-contents (dir)
  "Swap buffer in current window with buffer in the window in direction DIR."
  (let ((win (selected-window))
	(buf (current-buffer)))
    (windmove-do-window-select dir)
    (let ((targetbuf (current-buffer)))
      (set-window-buffer (selected-window) buf)
      (with-selected-window win
	(set-window-buffer (selected-window) targetbuf)))))

(defmacro define-move-window-contents-fn (dir)
  `(defun
       ,(intern (format "move-window-contents-%s" (symbol-name dir))) ()
     (interactive)
     (move-window-contents ',dir)))

;; (mapc #'define-move-window-contents-fn '(up down left right))
(define-move-window-contents-fn up)
(define-move-window-contents-fn down)
(define-move-window-contents-fn left)
(define-move-window-contents-fn right)


;;; Special case prefix args 2 and 3 since they're so common, and deserve their own keys without having to press the uarg key before them to initiate numeral entry. 4 is default for uarg, so it already doesn't require pressing an extra key. 5 and higher are infrequent enough that no special casing is needed; requiring uarg command prefix for them is ok.

;; Derived from simple.el
(defun universal-argument-x (&optional start)
  "Same as `universal-argument', but with optional START value instead of default of 4."
  (interactive)
  (setq prefix-arg (list (or start 4)))
  (universal-argument--mode))

;; Derived from simple.el
(defmacro define-universal-arg-funs (x)
  `(progn
     (defun ,(intern (format "universal-argument-%d" x)) ()
       (interactive)
       (universal-argument-x ,x))

     (defun ,(intern (format "universal-argument-more-%d" x)) (arg)
       (interactive "P")
       (setq prefix-arg (if (consp arg)
			    (list (* ,x (car arg)))
			  (if (eq arg '-)
			      (list (- ,x))
			    arg)))
       (when (consp prefix-arg) (universal-argument--mode)))))

(define-universal-arg-funs 2)
(define-universal-arg-funs 3)
;; 4 not needed here, since it's already hardcoded in simple.el


;;; Closed-buffer tracker. Inspired by:
;;; http://stackoverflow.com/questions/2227401/how-to-get-a-list-of-last-closed-files-in-emacs

(defvar closed-buffer-history nil
  "Reverse chronological list of closed buffers.
This list stores filenames and/or full buffer states as stored by `desktop-save-mode', including point, mark, and various other buffer-local variables.
The list size is limited by `closed-buffer-history-max-saved-items' and `closed-buffer-history-max-full-items'.
When a buffer already in the list is closed again, it's moved to the head of the list.")

(defvar closed-buffer-history-max-saved-items 1000
  "Max items to save on `closed-buffer-history' list.
Use -1 for unlimited, or zero to disable tracking closed files.
If disabled after having been enabled, `closed-buffer-history' will retain the list from when it was enabled, even though no new items will be added to it. To clear the list, set it to nil.
See also `closed-buffer-history-max-full-items'.")

(defvar closed-buffer-history-max-full-items 100
  "Max full items to save on `closed-buffer-history' list.
Use -1 for unlimited, or zero to disable tracking of full items. If this limit is less than `closed-buffer-history-max-saved-items', then non-full items will be stored for the difference. If this limit is greater, then `closed-buffer-history-max-saved-items' is the controlling limit. When new items are added to `closed-buffer-history', full items that exceed this limit are converted to non-full items. The purpose of that is to save space.
 A full item is a buffer state, including `buffer-file-name', `point', `mark', `mark-ring', `major-mode', minor modes, and various other buffer-local variables as configured for `desktop-save-mode', but excluding the buffer contents, which are stored only in the named file. A non-full item is just a file name.")

(defun untrack-closed-buffer (name)
  ;; Could be just name, or info list; delete in either case
  (setq closed-buffer-history
	(delete name
		(assoc-delete-all name closed-buffer-history))))

(defun track-closed-buffer ()
  (when (and buffer-file-name (not (= closed-buffer-history-max-saved-items 0)))
    ;; Remove from not-head of list
    (untrack-closed-buffer buffer-file-name)
    ;; Add to head of list
    (pushnew (if (desktop-save-buffer-p buffer-file-name (buffer-name) major-mode)
		 (cdr (save-current-buffer
			(let ((desktop-io-file-version (or desktop-io-file-version 208)))
			  (desktop-buffer-info (current-buffer)))))
	       buffer-file-name)
	     closed-buffer-history)
    ;; Truncate excess elements
    (let* ((max-full closed-buffer-history-max-full-items)
	   (max-saved closed-buffer-history-max-saved-items)
	   (truncatees (nthcdr max-saved closed-buffer-history))
	   demotees)
      (and (> max-saved 0) truncatees (setcdr truncatees nil))
      (unless (< max-full 0)
	(setq demotees (nthcdr max-full closed-buffer-history))
	;; Demote buffer info lists to filenames.
	(letrec ((demote (lambda (x) (when (and (consp x) (consp (car x)))
				       (setcar x (caar x)) (funcall demote (cdr x))))))
	  (funcall demote demotees))))))

(defun reopen-buffer (name &optional remove-missing select)
  "Open file, and restore buffer state if recorded in `closed-buffer-history'.
Return buffer for the opened file, or nil if not listed in `closed-buffer-history'.

If unable to open file, then remove from `closed-buffer-history' if confirmed
interactively or REMOVE-MISSING is non-nil, or signal error if it is
nil and reopen-buffer was not called interactively.

If called interactively, or SELECT is non-nil, then switch to the buffer."
  (interactive
   (list (completing-read "Last closed: "
			      (mapcar (lambda (x) (if (consp x) (car x) x))
				      closed-buffer-history)
			      nil t) nil t))
  (let* ((bufinfo (assoc name closed-buffer-history))
	 (bufinfo (or bufinfo (if (member name closed-buffer-history)
				  (make-list 8 nil)))))
    (assert bufinfo)
    ;;Load from info list, using base filename as new buffer name.
    (let ((buf
	   ;; Set variables needed by desktop-create-buffer.
	   ;; Need dlet because they're not globally special, but only locally
	   ;; special in desktop.el, which according to Stefan, is not weird.
	   (dlet ((desktop-buffer-ok-count 0)
		  (desktop-buffer-fail-count 0)
		  desktop-first-buffer)
		 (silently ; Silence desktop-restore-file-buffer if file can't be found
		  (apply 'desktop-create-buffer (string-to-number desktop-file-version)
			 name (file-name-nondirectory name) (cddr bufinfo))))))
      (if buf (progn
		(untrack-closed-buffer name)
		(with-current-buffer buf (run-hooks 'desktop-delay-hook))
		(setq desktop-delay-hook nil)
		(when select
		  ;; 3 lines copied from desktop-restore-file-buffer in desktop.el
		  (condition-case nil
		      (switch-to-buffer buf)
		    (error (pop-to-buffer buf))))
		buf)
	(when (or remove-missing
		  (and
		   (called-interactively-p 'any)
		   (y-or-n-p (format
			      "Failed to open file %s; remove from closed-buffer-history? "
			      name))))
	  (untrack-closed-buffer name))
	(unless (or remove-missing (called-interactively-p 'any))
	  (error "Failed to open file %s" name))))))

(unless (>= emacs-major-version 25)
  (defun reopen-buffer (&rest _dummy)
    (interactive)
    ;; Relies on Emacs commit# c96983efef17
    (user-error "reopen-buffer requires Emacs 25 or later")))


;;; Init

;;;###autoload
(defun usablizer-bind-keys ()
  "Hijack the user's keybindings."
  (interactive)
  (unless line-move-visual
    (user-error "usablizer-bind-keys is not designed for your weirdo config"))

  (mapc
   #'global-unset-key
   '(
     ;; Get rid of annoying new global keybindings in Emacs 24.3
     [XF86Back] ; previous-buffer
     [XF86Forward] ; next-buffer

     ;; Better bindings for these later in this function
     "\M-\d" ; backward-kill-word
     [C-backspace] ; backward-kill-word
     [menu] ; execute-extended-command
     [redo] ; repeat-complex-command
     [f4] ; kmacro-start-macro-or-insert-counter
     [C-up] ; Emacs's weird paragraph movement commands
     [C-down]))

  (vimizer-bind-keys) ; Emacs isn't usable without them
  (setq shift-select-mode nil) ; Get rid of the Windintosh junk

  (global-set-key-list
   '(

     ;;Movement commands
     ([S-up] previous-logical-line) ; Non-shifted up and down move by visual lines
     ([S-down] next-logical-line)
     ([S-prior] scroll-down-line)
     ([S-next] scroll-up-line)
     ([f20] forward-to-word)
     ([f17] reverse-rotate-mark-ring-and-point)
     ([S-f17] rotate-mark-ring-and-point)
     ([C-S-left] uf-backward-sexp) ; Using C-left and C-right for bw_word and end_wrd keys, so I get S-bw_word and S-end_wrd for uf-backward-sexp and uf-forward-sexp
     ([C-S-right] uf-forward-sexp)
     ([S-f20] forward-to-sexp)
     ([C-s-S-left] not-weird-beginning-of-defun)
     ([C-s-S-right] not-weird-end-of-defun)
     ([s-S-f20] forward-to-defun)
     ([s-up] not-weird-backward-paragraph)
     ([s-S-down] not-weird-forward-paragraph)
     ([s-down] forward-to-paragraph)
     ([C-s-left] backward-out-list)
     ([C-s-right] out-list)
     ([s-f20] in-list)
     ([S-home] home-list)
     ([S-end] end-list)
     ([s-S-home] beginning-of-visual-line)
     ([s-S-end] end-of-visual-line) ; FIXME (Emacs bug): works if word wrap enabled, but moves one too many chars if char wrap enabled and font is monospace.
     ([s-home] beginning-of-buffer)
     ([s-end] end-of-buffer)
     ;; Exclusive forward find-char paired with inclusive backward is intentional
     ([f19] find-char-exclusive)
     ([S-f19] backward-find-char-inclusive)
     ([s-f19] find-char-inclusive)
     ([s-S-f19] backward-find-char-exclusive)
     ([s-S-up] moveto-winline-1)
     ([s-S-left] recenter-top-bottom)
     ([C-home] UNUSED)

     ;;Editing commands
     ([C-backspace] backward-delete-word)
     ([S-backspace] backward-delete-word)
     ([S-delete] just-one-space-or-eol)
     ([s-delete] remove-all-text-properties)
     ("\t" completion-at-point) ; TODO: this gets rid of completion window if I use motion command, but not if I press escape. Get rid of it in latter case also. Other options would be hippie-expand or dabbrev-expand, but they don't use a full-window completion buffer like minibuffer-complete does.
     ([kp-tab] insert-tab-command)
     ([f21] indent-for-tab-command)
     ([S-f21] unindent)
     ([s-f21] indent-sexp-or-region)
     ([s-S-f21] indent-defun-or-region)
     ([S-return] electric-indent-just-newline)
     ([C-return] nl-under)
     ([C-S-return] nl-over)
     ([s-return] nl-under)
     ([s-S-return] nl-over)
     ([s-kp-enter] nl-under)
     ([s-S-kp-enter] nl-over)
     ;; ([kp-enter] UNUSED) ; TODO: implement fnenter with something like ffap, to follow intra- or inter-document links in text
     ;; ([S-kp-enter] UNUSED) ; TODO: implement something else useful
     ([XF86Spell] ispell-word)
     ([S-XF86Spell] toggle-letter-case)
     ([s-XF86Spell] ispell)
     ([s-S-XF86Spell] flyspell-mode-toggle)

     ;; File, buffer, and window management
     ([XF86Open] switch-to-buffer)
     ([S-XF86Open] find-file)
     ([s-XF86Open] ibuffer)
     ([s-S-XF86Open] bury-buffer)
     ([XF86Close] kill-buffer-no-ask)
     ([S-XF86Close] reopen-buffer)
     ([s-XF86Close] quit-popup)
     ([M-XF86Close] delete-window)
     ([M-S-XF86Close] kill-buffer-and-window)
     ([XF86Save] save-buffer)
     ([S-XF86Save] save-some-buffers)
     ([M-up] windmove-up)
     ([M-down] windmove-down)
     ([M-left] windmove-left)
     ([M-right] windmove-right)
     ([M-S-up] move-window-contents-up)
     ([M-S-down] move-window-contents-down)
     ([M-S-left] move-window-contents-left)
     ([M-S-right] move-window-contents-right)
     ([C-tab] other-window)
     ([C-S-iso-lefttab] other-window-back) ; Emacs calls S-tab ⌜S-iso-lefttab⌝
     ([M-prior] split-window-vertically)
     ([M-next] delete-other-windows-vertically)
     ([C-M-left] split-window-horizontally)
     ([C-M-right] delete-other-windows-horizontally) ; TODO: implement this
     ([M-f20] delete-other-windows) ; xmonad uses M-space, not M-f20
     ([M-undo] winner-undo) ; TODO: replace by better variant in workgroups2
     ([M-S-undo] winner-redo)

     ;; Miscellaneous
     ([S-help] eldoc-mode)
     ([s-help] UNUSED)
     ([s-S-help] UNUSED)
     ([s-XF86Xfer] insert-char)
     ([s-S-XF86Xfer] describe-char)
     ([s-S-right] set-line-wrap)
     ([C-end] UNUSED)
     ([f14] universal-argument)
     ([S-f14] universal-argument)
     ([s-f14] negative-argument) ; XXX: s-f14 n where n is a numeral produces the negative, as expected, unless n is 0, in which case it produces -1, due to brain damage in Emacs's universal argument processing (see digit-argument in simple.el)
     ([s-S-f14] negative-argument)
     ([f2] universal-argument-2)
     ([S-f2] universal-argument-2)
     ([f3] universal-argument-3)
     ([S-f3] universal-argument-3)
     ([menu] menu-bar-open)
     ([S-menu] count-words)
     ([s-S-menu] UNUSED)
     ([SunProps] execute-extended-command)
     ([S-SunProps] eval-region-or-last-sexp)
     ([s-SunProps] eval-expression)
     ([s-S-SunProps] shell)
     ;; http://www.freebsddiary.org/APC/usb_hid_usages.php calls code 0x79 "Again". Emacs binds it by default to repeat-complex-command, to which it also binds another key it calls "again". So why does it call 0x79 "redo"? XXX: check bindings.
     ([S-redo] kmacro-end-or-call-macro)
     ([S-XF86Tools] kmacro-start-macro-or-insert-counter) ; I.e. s-S-redo
     ;; Disable undo key if undo-tree mode is disabled, to avoid accidentally using Emacs's standard undo without realizing it
     ([undo] undo-tree-mode-not-enabled)
     ([S-undo] undo-tree-mode-not-enabled)
     ([s-undo] undo-tree-mode-not-enabled)
     ([s-S-undo] revert-buffer)
     ([f16] exchange-point-and-mark)
     ([S-f16] toggle-region-activation)
     ([s-f16] narrow-to-region-tweaked)
     ([s-S-f16] widen)
     ([XF86Go] jump-to-register)
     ([S-XF86Go] point-to-register)
     ([s-XF86Go] list-registers)
     ;; TODO change scrolling for undo-tree visualizer to use scroll-lock-mode, or at least stop scrolling conservatively. Just setting scroll-conservatively with let binding doesn't work; global value has to be set. Maybe using make-local-variable?
     ([Scroll_Lock] scroll-lock-mode) ; FIXME (Emacs bug): scroll-lock-mode doesn't work right on wrapped lines; point gets dragged. And scroll-lock-mode doesn't work in undo-tree visualizer.
     ([S-f11] check-parens-and-report)
     ([s-f11] show-paren-mode)
     ([s-S-f11] goto-next-overlong-line)
     ([s-next] UNUSED)
     ([s-prior] UNUSED)))

  (mapc
   (lambda (x) (define-key universal-argument-map (car x) (cadr x)))
   '(([f14] universal-argument-more)
     ([S-f14] universal-argument-more)
     ([f2] universal-argument-more-2)
     ([S-f2] universal-argument-more-2)
     ([f3] universal-argument-more-3)
     ([S-f3] universal-argument-more-3)))

  (define-key undo-tree-map [undo] 'undo-tree-undo)
  (define-key undo-tree-map [S-undo] 'undo-tree-redo)
  (define-key undo-tree-map [s-undo] 'undo-tree-visualize)
  (define-key calc-mode-map [undo] 'calc-undo)
  (define-key calc-mode-map [S-undo] 'calc-redo)
  (define-key calc-mode-map [XF86Paste] 'calc-yank)
  ;; Not needed: (define-key calc-dispatch-map "·" 'calc-same-interface)
  (define-key calc-mode-map "·" 'calc-times)
  (define-key calc-mode-map "÷" 'calc-divide)
  (define-key calc-mode-map "—" 'calc-inv)
  (define-key calc-mode-map "⅓" 'usablizer-calc-third)
  (define-key calc-mode-map "½" 'usablizer-calc-half)
  (define-key calc-mode-map "δ" 'usablizer-calc-double)
  (define-key calc-mode-map "τ" 'usablizer-calc-triple)
  (define-key calc-mode-map "√" 'calc-sqrt)
  (define-key calc-mode-map "∛" 'usablizer-calc-cube-root)
  (define-key calc-mode-map "²" 'usablizer-calc-sqr)
  (define-key calc-mode-map "³" 'usablizer-calc-cube)
  (define-key calc-digit-map "·" 'calcDigit-nondigit)
  (define-key calc-digit-map "÷" 'calcDigit-nondigit)
  (define-key calc-digit-map "—" 'calcDigit-nondigit)
  (define-key calc-digit-map "⅓" 'calcDigit-nondigit)
  (define-key calc-digit-map "½" 'calcDigit-nondigit)
  (define-key calc-digit-map "δ" 'calcDigit-nondigit)
  (define-key calc-digit-map "τ" 'calcDigit-nondigit)
  (define-key calc-digit-map "√" 'calcDigit-nondigit)
  (define-key calc-digit-map "∛" 'calcDigit-nondigit)
  (define-key calc-digit-map "²" 'calcDigit-nondigit)
  (define-key calc-digit-map "³" 'calcDigit-nondigit)
  (push '("·" "*") math-read-replacement-list)
  (push '("÷" "/") math-read-replacement-list)
  (push '("√" "sqrt") math-read-replacement-list)
  (push '("≠" "!=") math-read-replacement-list)


;;; Emacs: the customizable text editor, in roughly the same way that a brick wall is customizable. If you beat your head against it hard enough, you can actually shove it into a less obstructive form.

  ;; This STILL doesn't entirely work. jump-to-register → register-read-with-preview → read-key behaves differently for C-g vs. esc.
  (global-set-key [escape] 'dynamic-quit) ; Default binding of esc key is esc-map, which is ridiculous

  ;; (global-set-key [S-escape] UNUSED)
  (global-set-key [s-escape] 'not-annoying-keyboard-escape-quit)
  (global-set-key [s-S-escape] esc-map) ; In case esc-map is actually needed for something (unlikely)
  (define-key undo-tree-visualizer-mode-map [remap keyboard-quit] 'undo-tree-visualizer-quit) ;; TODO: check if I still need this after changing «escape» from keyboard-quit to dynamic-quit

  ;; FIXME: I want to send escape in term mode rather than interpret it in Emacs.
  ;; But the following doesn't work, and I don't know why:
  ;; (define-key term-mode-map [escape] 'term-send-raw-meta)

  ;; Use the «find» key for isearch
  ;; XXX; Can't do (global-set-key [find] "\C-s") because as http://www.xandr.net/.emacs says:
  ;;	 it does not currently work to say
  ;;	   (global-set-key 'f3 "\C-x\C-f")
  ;;	 The reason is that macros can't do interactive things properly.
  ;;	 This is an extremely longstanding bug in Emacs.
  ;; Yay, Emacs.
  (global-set-key [find] 'isearch-forward)
  (global-set-key [S-find] 'isearch-backward)

  ;; XXX: Manual section «(emacs) Init Rebinding» says I'm supposed to do this:
  ;; (add-hook 'minibuffer-setup-hook #'usablizer-fix-minibuffer-maps)
  ;; (add-hook 'isearch-mode-hook #'usablizer-fix-isearch-map)
  ;; But this seems to work just as well:
  (usablizer-fix-minibuffer-maps)
  (usablizer-fix-isearch-map)

  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cc" 'org-capture)
  (global-set-key "\C-cb" 'org-iswitchb))

;;;###autoload
(defun usablizer-init-essential ()
  "Do essential initialization for Emacs usability, to avoid some idiotic data-losing default behavior, and accidental data-munging in org-mode."
  (add-hook 'find-file-hook #'register-swap-back)
  (add-hook 'desktop-delay-hook #'add-register-swap-outs t) ; Can't use desktop-after-read-hook for register swap outs since buffers might be lazily restored. Since I'm using desktop-delay-hook, must append, so that the set-marker calls that are added to desktop-delay-hook when desktop-create-buffer runs are run first.
  (add-hook 'kill-buffer-hook #'track-closed-buffer)
  (setq org-catch-invisible-edits 'error)
  (setq org-ctrl-k-protect-subtree 'error)

  ;; Enable quit-popup
  (advice-add #'display-buffer :filter-return #'save-window-of-last-displayed-buffer))

;;;###autoload
(defun usablizer-init ()
  (usablizer-init-essential)
  (delete-selection-mode)
  (global-undo-tree-mode))

(usablizer-init)


(provide 'usablizer)

;;; usablizer.el ends here
