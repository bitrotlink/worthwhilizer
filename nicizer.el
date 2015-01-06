;;; nicizer.el --- Make Emacs nice -*- lexical-binding: t; -*-
;; Version: 0.1.1
;; Package-Requires: ((paredit FIXME) (usablizer "0.1.2"))

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
;; Minor modes which are normally on (with Nicizer, these include undo-tree-mode, whitespace-mode, and word-wrap in text-mode and prog-mode buffers) have their lighters hidden in the modeline when the modes are on, and have mode-off lighters shown when the modes are off. The latter feature compensates for the former, ensuring that the modeline is never ambiguous about which modes are on.
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
;; Since Emacs is a text editor, and text is usually more readable with variable-pitch fonts, Nicizer enables variable-pitch by default. Monospace mode is provided to compensate, and is enabled by default for major modes which need it.
;;
;; 4. Word-wrap enabled by default.
;; Sometimes you want char wrap or no wrap, but Emacs is a text editor; word wrap is the common case. For the uncommon cases, use set-line-wrap (from Usablizer).
;;
;; 5. Various Emacs settings and features enabled to make it nicer.
;; undo-tree-mode, electric-pair-mode, show-paren-mode, size-indication-mode, better settings for scrolling, etc.


;;; Code:

(require 'whitespace)
(require 'face-remap)
(require 'paredit)
(require 'usablizer)


;;; Utilities

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

;; TODO: Getting way too long; invert, and say only text-mode and prog-mode normally have whitespace-mode on.
(defvar whitespace-mode-normally-off
  '(help-mode term-mode shell-mode eshell-mode undo-tree-visualizer-mode diff-mode grep-mode occur-mode debugger-mode messages-buffer-mode package-menu-mode Info-mode Man-mode woman-mode ibuffer-mode calc-mode apropos-mode inferior-emacs-lisp-mode completion-list-mode)
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
			 undo-tree-off-lighter))

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

(defun unclutter-mode-line ()
  "Show when normally-on modes are off.
Show nothing when they're on, to avoid cluttering the mode line."
  (set-minor-mode-lighter 'undo-tree-mode undo-tree-maybe-lighter)
  (set-minor-mode-lighter 'whitespace-mode whitespace-mode-maybe-lighter)
  (set-minor-mode-lighter 'global-whitespace-mode "") ; Requires special-casing due to poor design of global-whitespace-mode in Emacs
  (mapc #'delete-from-mode-line quiet-lighters) ; Reset, in case they're already there
  (mapc #'insert-after-modes quiet-lighters) ; After, for emphasis, since normally silent
  (force-mode-line-update))

(defun clutter-mode-line ()
  "Undo `unclutter-mode-line'."
  (set-minor-mode-lighter 'undo-tree-mode
			  (eval (car (get 'undo-tree-mode-lighter 'standard-value))))
  (set-minor-mode-lighter 'whitespace-mode " ws") ; Standard value
  (set-minor-mode-lighter 'global-whitespace-mode " WS")
  (mapc #'delete-from-mode-line quiet-lighters)
  (force-mode-line-update))

;; TODO: switch to new advice system
(defun up-down-ring-isearch ()
  "Use up and down arrow keys after isearch to select a search string from the ring. This is more convenient than pressing M-p and M-n. Enable by adding this function to `isearch-mode-hook'."
  (unless (memq this-command ; Emacs is so gay
		'(isearch-forward-exit-minibuffer isearch-reverse-exit-minibuffer))
    (define-key isearch-mode-map [down] 'isearch-ring-advance)
    (define-key isearch-mode-map [up] 'isearch-ring-retreat)
    (add-hook 'post-command-hook 'maybe-remove-up-down-ring)))

;; Search ring movement no longer needed if user is typing a search string
(defun maybe-remove-up-down-ring ()
  (if (eq this-command 'isearch-printing-char)
      (remove-up-down-ring)))

(defun remove-up-down-ring ()
  (define-key isearch-mode-map [down] nil)
  (define-key isearch-mode-map [up] nil)
  (remove-hook 'post-command-hook 'maybe-remove-up-down-ring))

(defadvice isearch-repeat-forward (after no-up-down-ring-after-isearch-repeat)
  "After the first time isearch is repeated, remove up and down mapping for ring movement, so that up or down will exit isearch as is standard for Emacs, instead of trying to move in the search ring, which is no longer useful after isearch has been repeated (which means the user has already found the history item he was looking for)."
  (remove-up-down-ring))

(defadvice isearch-repeat-backward (after no-up-down-ring-after-isearch-repeat-1)
  "Supplement to no-up-down-ring-after-isearch-repeat"
  (remove-up-down-ring))

(defadvice isearch-forward-exit-minibuffer (before no-up-down-ring-after-isearch-repeat-2)
  "Supplement to no-up-down-ring-after-isearch-repeat"
  (remove-up-down-ring))

(defadvice isearch-reverse-exit-minibuffer (before no-up-down-ring-after-isearch-repeat-3)
  "Supplement to no-up-down-ring-after-isearch-repeat"
  (remove-up-down-ring))

;; Add this function to flyspell-mode-hook to flyspell buffer when enabling flyspell mode but not when disabling flyspell mode
;; Need this because flyspell-mode runs its hook both when turning on and when turning off
(defun maybe-flyspell-buffer ()
  (if flyspell-mode (flyspell-buffer)))

(defvar text-browse-minor-mode) ; Defined below; silence compiler warning here
;; Use block cursor when region is inactive, except in text-browse mode
;; Add this to deactivate-mark-hook
(defun nicizer-deactivate-mark ()
  (setq cursor-type (if text-browse-minor-mode nil t)))

;; Use bar cursor when region is active, except in line-select mode
;; Add this to activate-mark-hook
(defun nicizer-activate-mark ()
  ;; line-select is in Vimizer
  (setq cursor-type (if (bound-and-true-p line-select-minor-mode) nil 'bar)))


;;; Fix whitespace-mode brokenness

(defvar whitespace-mode-actually-on nil
  "t if whitespace mode is on.
This is unlike Emacs's `whitespace-mode' variable, which isn't set by turning on whitespace mode globally.")
(make-variable-buffer-local 'whitespace-mode-actually-on)
(put 'whitespace-mode-actually-on 'permanent-local t) ; Necessary for accuracy because whitespace mode remains on in a buffer after changing major mode if global-whitespace-mode is on. FIXME: or does whitespace-turn-on get called again after the change of major mode, so it isn't actually necessary to have this permanent-local?

(defadvice whitespace-turn-on (after record-whitespace-mode-actually-on)
  (setq whitespace-mode-actually-on t))

(defadvice whitespace-turn-off (after record-whitespace-mode-actually-off)
  (setq whitespace-mode-actually-on nil))

(ad-activate 'whitespace-turn-on)
(ad-activate 'whitespace-turn-off)


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
An interface to `buffer-face-mode' which uses `monospace-mode-face'.
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

;; Emacs's view-mode implements a bunch of keys which I neither need nor want, and doesn't implement some that I do (e.g. up and down arrows scroll the window rather than move the cursor), and doesn't hide the cursor, or enable word-wrap. Text-Browse minor mode solves this.
(defvar tbmm-original-read-only nil)
(defvar tbmm-original-show-paren nil)
(defvar tbmm-original-line-wrap nil)
(make-variable-buffer-local 'tbmm-original-read-only)
(make-variable-buffer-local 'tbmm-original-show-paren)
(make-variable-buffer-local 'tbmm-original-line-wrap)

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
	(read-only-mode)
	(setq tbmm-original-show-paren show-paren-mode)
	(show-paren-mode 0)
	(setq tbmm-original-line-wrap (get-line-wrap))
	(set-line-wrap 'word)
	(unless mark-active (setq cursor-type nil)))
    (progn
      (read-only-mode (or tbmm-original-read-only 0)) ; Emacs's demented API
      (show-paren-mode (or tbmm-original-show-paren 0))
      (set-line-wrap tbmm-original-line-wrap)
      (unless (bound-and-true-p line-select-minor-mode)
	(setq cursor-type (if mark-active 'bar t))))))

(defun text-browse-minor-mode-toggle ()
  "Toggle `text-browse-minor-mode' without superfluous message'." ; Mode is already indicated on the mode line
  (interactive)
  (text-browse-minor-mode (if text-browse-minor-mode 0 t)))


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
  (text-browse-minor-mode 0)
  (read-only-mode 0))


;;; Init

;;;###autoload
(defun nicizer-init ()
  "Hijack the user's settings and keybindings.
Add hooks to set bar cursor when mark is active, and block cursor when mark is inactive.
Show whitespace, set variable-pitch font, unclutter the mode line.
Many others."
  (interactive)
  ;; usablizer-bind-keys omitted here because everybody would whine about it
  (vimizer-bind-keys)
  (global-set-key-list
   `(([M-S-f13] paredit-mode)

     ;; Some paredit commands useful even when not in paredit mode
     ([f14] paredit-backward-slurp-sexp)
     ([f13] paredit-forward-slurp-sexp)
     ([S-f14] paredit-backward-barf-sexp)
     ([S-f13] paredit-forward-barf-sexp)
     ([M-f14] paredit-split-sexp)
     ([M-S-f14] paredit-join-sexps)
     ([M-f13] paredit-raise-sexp)
     (,(kbd "M-(") paredit-wrap-round)
     (,(kbd "M-[") paredit-wrap-square)
     (,(kbd "M-)") paredit-splice-sexp)

     ;; Miscellaneous
     ([S-XF86Forward] monospace-mode)
     ([M-menu] text-browse-minor-mode-toggle)
     ([M-XF86Back] zoom-standard)
     ([M-S-XF86Back] zoom-out)
     ([M-S-XF86Forward] zoom-in)))

  ;; Better access to the search history ring
  (ad-activate 'isearch-repeat-forward)
  (ad-activate 'isearch-repeat-backward)
  (ad-activate 'isearch-forward-exit-minibuffer)
  (ad-activate 'isearch-reverse-exit-minibuffer)
  (add-hook 'isearch-mode-hook 'up-down-ring-isearch)
  ;; To prevent maybe-remove-up-down-ring from being left in post-command-hook
  (add-hook 'isearch-mode-end-hook 'remove-up-down-ring)

  ;; Add delimiter pairs missing from Emacs
  ;; For electric-pair-mode, show-paren-mode, and sexp-based movement/editing commands
  (mapc (lambda (x)
	  (set-case-syntax-delims (car x) (cadr x) (standard-case-table)))
	'((?‹ ?›)
	  (?❬ ?❭)
	  (?❮ ?❯)
	  (?« ?») ; This was removed from Emacs 24.4 in deference to Germans, who are backward
	  (?‟ ?”) ; U201F, not U201C, is the horizontal-reverse of U201D
	  (?⌜ ?⌝) ; Quasi-quotation marks
	  (?⌞ ?⌟)
	  (?⎡ ?⎤) ; Integer ceiling
	  (?⎣ ?⎦))) ; Floor

  ;; Improved settings
  (setq inhibit-startup-screen t)
  (setq initial-scratch-message nil)
  (fset 'display-startup-echo-area-message (lambda nil nil)) ; (setq inhibit-startup-echo-area-message t) doesn't suffice, due to the ‟weirdly extreme treatment” described for it in startup.el
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
  (column-number-mode)
  (size-indication-mode)
  (electric-pair-mode)
  (show-paren-mode)
  (winner-mode)
  (delete-selection-mode) ; XXX: Really belongs in usablizer
  (global-undo-tree-mode) ; XXX: Ditto
  (blink-cursor-mode 0)
  (menu-bar-mode 0) ; Permanent menu bar is pointless; use menu-bar-open
  (tool-bar-mode 0)
  (put 'narrow-to-region 'disabled nil)
  (unclutter-mode-line) ; To undo this, use: (clutter-mode-line)

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

  (add-hook 'message-mode-hook (lambda () (auto-fill-mode 0))) ; FIXME: insert (at front) hook to message-send-mail-function to check for lines longer than 1000 chars, and offer to do hard word wrap before sending. And test this.

;; Highlight erroneous whitespace
  ;; But if user already customized, then don't override that or turn on globally
  (if (maybe-set 'whitespace-style
		 '(face empty trailing space-before-tab::tab space-after-tab::tab))
      (global-whitespace-mode))

  ;; Use variable-pitch font by default
  (when (string= (face-attribute 'default :family) "DejaVu Sans Mono") ; Don't override user's custom config
    (set-face-attribute 'default nil :family "Sans Serif") ; Or "DejaVu Serif"
    (mapc (lambda (x) (add-hook x 'monospace-mode))
	  '(term-mode-hook
	    dired-after-readin-hook ; Because dired-mode uses spaces for column alignment, instead of using tabulated-list-mode
	    ibuffer-mode-hook ; Ditto
	    shell-mode-hook ; Because many commands' output formats, e.g. from ⌜ls -l⌝, have the same problem that dired-mode has
	    eshell-mode-hook ; Ditto
	    prog-mode-hook))) ; Sop to the luddites

  (mapc (lambda (x) (add-hook (car x) (cadr x)))
	'((buffer-face-mode-hook set-monospace-mode-lighter)
	  (deactivate-mark-hook nicizer-deactivate-mark)
	  (activate-mark-hook nicizer-activate-mark)
	  (rotate-mark-ring-hook track-mark-ring-position) ; See Usablizer
	  (flyspell-mode-hook maybe-flyspell-buffer))))


(provide 'nicizer)

;;; nicizer.el ends here
