;;; nicizer.el --- Make Emacs nice -*- lexical-binding: t; -*-
;; Version: 0.2.7
;; Package-Requires: ((undo-tree "0.6.5") (vimizer "0.2.6") (usablizer "0.2.5"))

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
(require 'ido)
(require 'dired-x) ; For dired-jump
(load "cl-seq") ; For the CL «position» and «intersection»
(eval-and-compile
  (require 'cl) ; Load-time too, for ⌜position⌝ and ⌜intersection⌝ aliases
  (require 'vimizer)) ; For the «silently» macro, and global-set-key-list
(require 'undo-tree)
(require 'workgroups)  ; TODO: Switch to workgroups2
(require 'paredit) ; TODO: maybe smartparens instead
(require 'expand-region) ; TODO: maybe something else
(require 'highlight-symbol) ; TODO: probably not
(require 'usablizer)

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

;; Necessary to enable the lighters to work:
(put 'undo-tree-off-lighter 'risky-local-variable t)
(put 'undo-tree-maybe-lighter 'risky-local-variable t)
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

;; Add this function to flyspell-mode-hook to flyspell buffer when enabling flyspell mode but not when disabling flyspell mode
;; Need this because flyspell-mode runs its hook both when turning on and when turning off
(defun maybe-flyspell-buffer ()
  (if flyspell-mode (flyspell-buffer)))

(defun ido-disable-line-trucation () (set (make-local-variable 'truncate-lines) nil))

;; The following enables pressing SunOpen twice to create and switch to a new buffer, and S-SunOpen twice to do dired-jump. (Usablizer's related global keybindings enable pressing SunOpen once to open buffer switcher, or S-SunOpen once to open file finder.)
(defun nicizer-ido-keys () ; Add this to ido-setup-hook
;;  (define-key ido-completion-map '[SunOpen] 'ido-enter-find-file) ; Don't want after all
  (define-key ido-completion-map '[SunOpen] 'switch-to-new-buffer)
  (define-key ido-completion-map '[S-SunOpen] 'dired-jump-from-ido))

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
						     ;; f1-f9 for 1-9; f10 for 0
						     (if (= i 0) 10 i)))
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


;;; Fix whitespace-mode brokenness

(defvar-local whitespace-mode-actually-on nil
  "t if whitespace mode is on.
This is unlike Emacs's `whitespace-mode' variable, which isn't set by turning on whitespace mode globally.")
(put 'whitespace-mode-actually-on 'permanent-local t) ; Necessary for accuracy because whitespace mode remains on in a buffer after changing major mode if global-whitespace-mode is on. FIXME: or does whitespace-turn-on get called again after the change of major mode, so it isn't actually necessary to have this permanent-local?

(defun whitespace-turn-on--record-actually-on ()
  (setq whitespace-mode-actually-on t))

(defun whitespace-turn-off--record-actually-off ()
  (setq whitespace-mode-actually-on nil))

(advice-add #'whitespace-turn-on :after #'whitespace-turn-on--record-actually-on)
(advice-add #'whitespace-turn-off :after #'whitespace-turn-off--record-actually-off)


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
  (text-browse-minor-mode 0)
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
      ;; If running from ido, can't just switch to buffer, because aborting recursive edit overrides buffer selection on exit, and I can't just exit-recursive-edit since that'll just accept ido's default and cause ido to override my buffer switch, so I have to switch after aborting. This foobarness is all because I'm putting this function in ido-completion-map so that I can press SunOpen twice to get a new buffer (once is to run ido-switch-buffer), and ido is just so beautifully elegant in the way it handles that.
      (if (= (minibuffer-depth) 0) (switch-to-buffer nb)
	(add-hook 'post-command-hook gross-hack)
	;; FIXME: messages "Quit", and I don't think I can avoid it without changing the C code.
	(abort-recursive-edit)))))

;;ido-dired isn't suitable for this, since it doesn't jump to line of current file.
(defun dired-jump-from-ido ()
  "`dired-jump', after working around ido awkwardness.
See comments in code for `switch-to-new-buffer' for details."
  (interactive)
  (letrec ((gross-hack-dired-ido
	    (lambda () (dired-jump)
	      (remove-hook 'post-command-hook gross-hack-dired-ido))))
    (add-hook 'post-command-hook gross-hack-dired-ido)
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


;;; Init

;;;###autoload
(defun nicizer-bind-keys ()
  "Hijack the user's keybindings."
  (interactive)
  (global-set-key-list
   `(([M-S-f13] paredit-mode)

     ;; Some paredit commands useful even when not in paredit mode
     ([f13] paredit-forward-slurp-sexp)
     ([S-f13] paredit-forward-barf-sexp)
     ([f14] paredit-backward-slurp-sexp)
     ([S-f14] paredit-backward-barf-sexp)
     ([M-f14] paredit-split-sexp)
     ([M-S-f14] paredit-join-sexps)
     ([M-f13] paredit-raise-sexp)
     (,(kbd "M-(") paredit-wrap-round)
     (,(kbd "M-[") paredit-wrap-square)
     (,(kbd "M-)") paredit-splice-sexp)

     ;; Miscellaneous
     ([M-menu] text-browse-minor-mode-toggle)
     ([S-menu] nicizer-kbd-previous-layout)
     ([M-S-menu] nicizer-kbd-switch-layout)
     ([S-f10] nicizer-kbd-layout-reset)
     ([M-XF86Save] enable-read-write)
     ([S-XF86Forward] monospace-mode)
     ([M-XF86Forward] calc)
     ([M-S-XF86Forward] zoom-in)
     ([M-XF86Back] zoom-standard)
     ([M-S-XF86Back] zoom-out)
     ([C-SunFront] er/expand-region) ; selsexp key is C-SunFront ; TODO: replace this?
     ([s-S-SunOpen] wg-switch-to-previous-workgroup)
     (,(kbd "s-U") wg-switch-to-previous-workgroup)
     ([C-S-f15] ,ctl-x-map) ; genprfx. FIXME: C-S-F15 C-t does transpose-lines, as expected, but in calc mode it still tries to do transpose-lines (which doesn't work), instead of calc-transpose-lines that C-x C-t is rebound to. Is calc-mode remapping the keychord instead of the function? And where in the Emacs source code C-x is bound to ctl-x-map?
     ([C-M-f15] mode-specific-command-prefix) ; mdeprfx. FIXME: Emacs's bindings.el does (define-key global-map "\C-c" 'mode-specific-command-prefix), and C-c works, but C-M-f15 doesn't work, at least for winner mode.
     ([s-f23] conlock)
     ([M-S-delete] copy-last-message)
     ([f8] nicizer-reset-stopwatch)
     ([f9] nicizer-read-stopwatch)

     ;; Replace Vimizer and Usablizer bindings to reduce message noise
     ([SunFront] silent-push-mark-command)
     ([M-home] silent-beginning-of-buffer)
     ([M-end] silent-end-of-buffer)))

  (nicizer-bind-wg-switch-keys)

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
  (add-hook 'isearch-mode-end-hook #'remove-up-down-ring))

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
  (delete-selection-mode) ; XXX: Really belongs in Usablizer
  (global-undo-tree-mode) ; XXX: Ditto
  (dynamic-cursor-mode)
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
		 '(face empty trailing space-before-tab::tab space-after-tab::tab))
      (global-whitespace-mode))

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
  (add-hook 'flyspell-mode-hook #'maybe-flyspell-buffer))

;;;###autoload
(defun nicizer-init-niche ()
  "Settings that you probably don't want."
  (remove-hook 'prog-mode-hook 'monospace-mode) ; Remove the sop to the luddites
  (add-hook 'find-file-hook (lambda () (setq buffer-read-only t))) ; Avoid accidentally modifying stuff
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
  (set-face-attribute 'default nil :height 90) ; Emacs default is 98, which is too big. Smaller increments are:
  ;; 90, which has monospace char size 7x15 pixels, proportional size 3-11x15 (width 3 for i and j, 11 for W).
  ;; 83, which has monospace char size 7x14 pixels, proportional size 3-9x15 (width 3 for i and j, 9 for W).
  ;; 75, which is still readable but too small for comfort.
  (set-scroll-bar-mode nil) ; TODO: have (thin) scroll bars, but overlay them with the gutters, rather than put them parallel to the gutters. This avoids wasting space. Use just a lightweight-shaded block for the scroll bar's slider to avoid obscuring the markings shown in the gutters.
  ;; (add-hook 'emacs-lisp-mode-hook #'turn-on-eldoc-mode)
  ;; (add-hook 'lisp-interaction-mode-hook #'turn-on-eldoc-mode)
  ;; (add-hook 'ielm-mode-hook #'turn-on-eldoc-mode)
  (set-case-syntax-delims ?§ ?‡ (standard-case-table))
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

  (ido-mode t)
  (add-hook 'ido-setup-hook #'nicizer-ido-keys)
  (setq ido-default-file-method 'selected-window)
  (setq ido-default-buffer-method 'selected-window)
  ;; (setq ido-use-virtual-buffers t) ; Don't like after all; when I close a buffer, I want it off my working list
  (add-hook 'ido-minibuffer-setup-hook #'ido-disable-line-trucation)
  ;; Copied from http://www.emacswiki.org/cgi-bin/wiki/InteractivelyDoThings
  ;; But this is still lame; even for long lists, it uses a tiny area (the minibuffer) and makes me scroll a lot, rather than using the full vertical screen space, or the full screen.
  ;; Display ido results vertically, rather than horizontally
  (setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))

  (eval-and-compile (require 'bookmark+)) ; TODO: Review
  (setq bookmark-default-file (concat user-emacs-directory "bookmarks"))
  (setq bookmark-save-flag nil)

  ;; TODO: Switch to workgroups2
  (setq wg-prefix-key '[C-f15])
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
  (add-hook 'desktop-after-read-hook #'conditionally-add-desktop-autosave)

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
  (fset 'message-make-date 'nicizer-message-make-date-UTC))

;;;###autoload
(defun nicizer-init-stateful ()
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

  (setq ido-save-directory-list-file (concat user-emacs-directory "ido.last"))

  (add-hook 'post-self-insert-hook #'typing-burst-timer)
  (add-hook 'post-command-hook #'typing-burst-typo-counter)
  (add-hook 'kill-emacs-hook #'dump-typing-burst-history)

  (eval-and-compile (require 'ecomplete))
  (setq ecomplete-database-file (concat user-emacs-directory "ecompleterc"))
  (setq message-mail-alias-type 'ecomplete)

  (pushnew 'closed-buffer-history desktop-globals-to-save)
  (pushnew 'closed-buffer-history-max-saved-items desktop-globals-to-save)
  (pushnew 'closed-buffer-history-max-full-items desktop-globals-to-save))

;;;###autoload
(defun nicizer-init-all ()
  "Initialize all keybindings and features of Vimizer, Usablizer, and Nicizer."
  (interactive)
  (usablizer-bind-keys) ; Also calls vimizer-bind-keys
  (nicizer-bind-keys)
  (nicizer-init)
  (nicizer-init-niche)
  (nicizer-init-stateful))


(provide 'nicizer)

;;; nicizer.el ends here
