
;; ****************************
;; *                          *
;; * USER-SET INITIALIZATIONS *
;; *                          *
;; ****************************

(server-start)

(setq inhibit-startup-screen t)

(global-font-lock-mode t)
;(electric-pair-mode 1)

(setq transient-mark-mode t)
(setq x-select-enable-clipboard t)
(delete-selection-mode 1)

(setq backup-by-copying-when-linked t)  ;; don't replace hard links
(setq use-file-dialog t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq require-final-newline nil)
(setq read-buffer-completion-ignore-case t)      ;; not needed with ido
(setq read-file-name-completion-ignore-case t)   ;; not needed with ido

(global-linum-mode t)
(setq column-number-mode t)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(setq scroll-bar-mode-explicit t)
(set-scroll-bar-mode `right)
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)
;(setq shift-select-mode nil)

(setq scroll-preserve-screen-position 'always)
(setq hscroll-margin 5)
(setq hscroll-step 1)

(setq-default indent-tabs-mode nil)
(icomplete-mode 99)
;; (iswitchb-mode 1)
(setq-default c-basic-offset 4)

(setq default-tab-width 4)
(setq fill-column 78)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
(add-to-list 'Info-default-directory-list "~/.emacs.d/info")

(setq ediff-split-window-function 
      (if (> (frame-width) 140) 'split-window-horizontally 
	'split-window-vertically))

(if (boundp 'cua-enable-cua-keys)
	(setf cua-enable-cua-keys nil))

;; **********************
;; *                    *
;; * INITIALIZE PLUGINS *
;; *                    *
;; **********************

(eval-when-compile (require 'subr-x))

(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/plugins")
(add-to-list 'load-path "~/.emacs.d/dash.el")

(require 'dash)
(require 'dash-functional)
(eval-after-load "dash" '(dash-enable-font-lock))

(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; ;; Cask & Pallet
;; (require 'cask)
;; (cask-initialize)

;; El-Get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(el-get 'sync)

;; undo-hist
(require 'undohist)
(undohist-initialize)

;; *************
;; *           *
;; * LIBRARIES *
;; *           *
;; *************

;; dash.el
(add-to-list 'load-path "~/.emacs.d/dash.el")


;; ***********
;; *         *
;; *  NOTES  *
;; *         *
;; ***********

;; M-x ffap  --  "find file at point" finds an elisp file and opens it (works in require).
;; M-x find-library  --  finds an elisp file and opens it (defaults to file at point if in require).
;; M-x locate-library  --  prompts for library name and prints address of file.

;; to byte (re)compile .emacs.d:
;;   (byte-recompile-directory "/home/troy/.emacs.d" 0 t)


;; ************************
;; *                      *
;; * PERSONAL ELISP FILES *
;; *                      *
;; ************************

;; non-package elisp:

(setq aux-elisp-files
      '("~/.emacs.d/lisp/troyp/utils.el"
		"~/.emacs.d/lisp/smooth-scrolling.el"
		"~/.emacs.d/lisp/move-text.el"
		"~/.emacs.d/lisp/search-bindings.el"
		))
(loop for f in aux-elisp-files do
      (if (file-exists-p f) (load f)))

;; packages:

(add-to-list 'load-path "~/.emacs.d/lisp/troyp")
(require 'mode-ring)
(require 'asciiheadings)


;; ===========================================================================
;; ****************
;; *              *
;; *  APPEARANCE  *
;; *              *
;; ****************
;; ---------------------------------------------------------------------------
;; ,-----------------,
;; | Theme variables |
;; '-----------------'

(setq dark-themes
	  '(
		zenburn
		hc-zenburn
		misterioso
		sanityinc-tomorrow-night
		sanityinc-tomorrow-eighties
		))
(setq light-themes
      '(
		sanityinc-solarized-light
		dichromacy
		adwaita
		sanityinc-tomorrow-day
		))
(setq mode-line-themes
      '(
		smart-mode-line-light
		smart-mode-line-dark
		))

(setq primary-light-theme 'adwaita)
(setq primary-dark-theme 'hc-zenburn)

(setq troy/white-grey "#F7F7F7")
(setq troy/dark-grey "#313131")

;; ---------------------------------------------------------------------------
;; ,-----------------,
;; | Theme functions |
;; '-----------------'

(defun light-theme-p (theme)
  (memq theme light-themes))
(defun dark-theme-p (theme)
  (memq theme dark-themes))
(defun mode-line-theme-p (theme)
  (memq theme mode-line-themes))

(defun cursor-color (col)
  (interactive (list (read-color "Cursor color: ")))
  ;; emacs mode color
  (set-cursor-color col)
  ;; evil-mode color
  (setq evil-default-cursor `(t ,col)))

(defun current-theme ()
  (car (--drop-while (mode-line-theme-p it) theme-history)))

(defun toggle-theme ()
  (interactive)
  (if (light-theme-p (current-theme))
      (progn
		(load-theme primary-dark-theme)
		(cursor-color troy/white-grey))
	(progn
	  (load-theme primary-light-theme)
	  (cursor-color troy/dark-grey))))
(defalias 'tt 'toggle-theme)

(setq theme-history nil)
(defadvice load-theme 
  (after push-history (THEME &optional NO-CONFIRM NO-ENABLE) activate)
  "Push THEME onto `theme-history' (init.el)"
  (setq theme-history (cons THEME theme-history)))

(load-theme primary-dark-theme t)
(load-theme 'smart-mode-line-light t)

;; ---------------------------------------------------------------------------
;; ,--------,
;; | Fringe |
;; '--------'

;; Fringe:
;; (set-face-attribute 'fringe nil :background "#555")  ;; set fringe to dark gray
(set-fringe-mode '(1 . 1))  ;; set fringe to 1px at left and right

;; ---------------------------------------------------------------------------
;; ,----------------,
;; | Variable Pitch |
;; '----------------'
;; C-x C-9

(defun set-vfont ()
  (interactive)
  (overlay-put (make-overlay (point-min) (point-max) nil nil t)
               'face '(:family "Century Schoolbook L")))

(defun set-mode-to-vpitch (hook)
  (add-hook hook (lambda ()
		   (variable-pitch-mode t)
		   (set-vfont))))

;; (dolist (hook '(erc-mode-hook
;; 		LaTeX-mode-hook
;; 		org-mode-hook
;; 		edit-server-start-hook
;; 		markdown-mode-hook
;; 		info-mode-hook))
;;   (set-mode-to-vpitch hook))

;; ---------------------------------------------------------------------------
;; ,-----------------,
;; | Smart Mode Line |
;; '-----------------'

(sml/setup)
(setq sml/replacer-regexp-list
      '(("^~/org/" ":Org:")
	("^~/\\.emacs\\.d/" ":Em.d:")
	("^/sudo:.*:" ":SU:")
	("^~/Documents/" ":Doc:")
	;("^~/[Gg]it/" ":Git:")
	;("^~/[Gg]it[Hh]ub/" ":Git:")
	;("^~/[Gg]it\\([Hh]ub\\|\\)-?[Pp]rojects/" ":Git:")
	("^~/Downloads/" ":DL:")
	("^~/\\.config/" ":Conf:")
	("^~/code/" ":CODE:")))

;; ---------
;; uniquify.
;; ---------
(require 'uniquify)    ;; remove in emacs24.4
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; ------
;; imenu.
;; ------
(defun try-to-add-imenu ()
  (condition-case nil (imenu-add-to-menubar "imenu") (error nil)))
(add-hook 'font-lock-mode-hook 'try-to-add-imenu)


;; ***********
;; *         *
;; *  INPUT  *
;; *         *
;; ***********
(set-input-method 'TeX)
(defun load-math-input ()
  (interactive)
  (load "~/.emacs.d/math-input.el"))

;; (require 'ibus)
;; ;; WARNING: ibus-mode steals "r" key from undo-tree
;; (add-hook 'after-init-hook 'ibus-mode-on)
;; (setq ibus-cursor-color '("red" "blue"))

;; -----
;; mozc.
;; -----
(require 'mozc)
(setq default-input-method "japanese-mozc")
(setq mozc-candidate-style 'overlay)
;; TODO:
;; C-\ toggle-input-method isn't turning off japanese input.
;; workaround: bind 'mozc-mode to C-\ instead.
;; This still doesn't work properly in emacs-mode. 
;; You still have to manually execute M-x mozc-mode to turn off japanese input.
;; However, it works in evil normal/insert modes.
(global-set-key (kbd "C-\\") 'mozc-mode)
;; (global-set-key (kbd "C-\\") 'toggle-input-method)
;; Defined in evil keybindings section
;; (define-key evil-normal-state-map (kbd "C-\\") #'mozc-mode)
;; (define-key evil-insert-state-map (kbd "C-\\") #'mozc-mode)


;; *********
;; *       *
;; * REGEX *
;; *       *
;; *********

(setq search-whitespace-regexp nil)  ;; treat spaces normally in interactive regex

;; http://www.emacswiki.org/emacs/rx
(defmacro rx-extra (&rest body-forms)
  (let ((add-ins (list
				  `(file . ,(rx (+ (or alnum digit "." "/" "-" "_"))))
				  `(ws0 . ,(rx (0+ (any " " "\t"))))
				  `(ws+ . ,(rx (+ (any " " "\t"))))
				  `(int . ,(rx (+ digit))))))
	`(let ((rx-constituents (append ',add-ins rx-constituents nil)))
	   ,@body-forms)))
;; ; example 
;; (let ((string " at Isrc/file-23_2.c line 23 ;: flubber"))
;;   (if (string-match (rx-extra (rx ws+ "at" ws+ (group file) ws+ "line" ws+ (group int))) string)
;;       (format "file is %s line is %s"
;;               (match-string-no-properties 1 string)
;;               (match-string-no-properties 2 string))))


;; **************************
;; *                        *
;; * Searching & NAVIGATION *
;; *                        *
;; **************************

;; --------
;; isearch.
;; --------
;; http://www.emacswiki.org/emacs/CaseFoldSearch
(add-hook
 'isearch-mode-hook
 (function
  (lambda ()
	(define-key isearch-mode-map "\C-h" 'isearch-mode-help)
	(define-key isearch-mode-map "\C-t" 'isearch-toggle-regexp)
	(define-key isearch-mode-map "\C-c" 'isearch-toggle-case-fold)
	(define-key isearch-mode-map "\C-j" 'isearch-edit-string))))
;; add indicator for case-fold-search in modeline
(add-to-list 'minor-mode-alist '(case-fold-search " CFS"))

;; -----------------
;; highlight-symbol.
;; -----------------
(require 'highlight-symbol)
(global-set-key [(control f3)] 'highlight-symbol-at-point)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(shift meta f3)] 'highlight-symbol-query-replace) ; default:(meta f3)

;; -----------------
;; goto-last-change.
;; -----------------
(require 'goto-last-change)
(global-set-key "\C-x\C-\\" 'goto-last-change)

;; -----------------
;; multiple-cursors.
;; -----------------
(require 'multiple-cursors)
;; note: return/C-g cancels; to enter a newline, use C-j
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;; **************
;; *            *
;; * COMPLETION *
;; *            *
;; **************

;; ----
;; helm
;; ----
(require 'helm)
(require 'helm-config)
(setq helm-quick-update t
      helm-idle-delay 0.01
      helm-input-idle-delay 0.01)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-M-i") 'helm-select-action)
(setq helm-ff-file-name-history-use-recentf t)

;; ---
;; ido
;; ---
(ido-mode t)
(setq ido-enable-flex-matching t)
;; ido:
;;   C-s,C-r: cycle completions
;;   TAB: show list of clickable completions
;;   BACKSPACE: back one directory
;;   M-b: first press removes / so you can edit the directory name (backspace
;;        works normally). Thereafter, removes one directory level.
;;   M-f: reverses one M-b
;;   C-z: reverses "merge" (directory shifting)
(defun bind-ido-keys ()
  "ido-mode keybindings"
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match) 
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match) 
  (define-key ido-completion-map (kbd "C-S-t") 'ido-toggle-prefix) 
  (define-key ido-common-completion-map (kbd "C-x <C-return>") 'ido-take-first-match)
  (define-key ido-completion-map (kbd "C-x <C-return>") 'ido-take-first-match) 
  (define-key ido-completion-map (kbd "<f1>") '(lambda () (interactive) (describe-function 'ido-find-file))))
(add-hook 'ido-setup-hook #'bind-ido-keys)


(require 'smex)
;; (autoload 'smex "smex" "Smex provides an ido interface to M-x commands.")
(global-set-key (kbd "M-x") 'smex)

;; (load "~/.emacs.d/ido-helm.el")
;; ;; (define-key ido-buffer-completion-map (kbd "<tab>") 'ido-next-match)
;; ;; (define-key ido-buffer-completion-map (kbd "<backtab>") 'ido-prev-match)


;; ;; anything
;; (require 'anything)

;; ---------
;; yasnippet
;; ---------
;; load before auto-complete
(require 'yasnippet)
(yas-global-mode 1)
(setq yas-triggers-in-field t)  ; allow stacked expansion
;; disable TAB for snippet expansion (replace with insert-mode_<C-.>)
(eval-after-load "yasnippet"
  '(progn
     (define-key yas-minor-mode-map [(tab)]        nil)
     (define-key yas-minor-mode-map (kbd "TAB")    nil)
     (define-key yas-minor-mode-map (kbd "<tab>")  nil)))


;; ------------
;; auto-complete
;; ------------
;; load AFTER yasnippet
(require 'auto-complete)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
(add-hook 'prog-mode-hook (lambda () (auto-complete-mode t)))
(ac-set-trigger-key "<tab>")

;; --------
;; semantic
;; --------
(add-hook 'c++-mode-hook
	  (lambda ()
	    (semantic-mode t)
	    (global-set-key (kbd "s-/") 'semantic-ia-complete-symbol)))


;; ****************************
;; *                          *
;; * CODE SEARCH & NAVIGATION *
;; *                          *
;; ****************************

(defalias 'findlib 'find-library)

;; ---------------------------------------------------------------------------
;; ,-------,
;; | Etags |
;; '-------'

;; <Emacs mode>___M-.        :  find-tags
;; <Normal mode>__<Leader>.  :  find-tags

(defun build-tags (dir)
  (interactive "DDirectory: ")
  (shell-command
   (concat "etags -R " dir)))

;; ---------------------------------------------------------------------------
;; ,-------,
;; | Eldoc |
;; '-------'

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; ---------------------------------------------------------------------------
;; ,---------------,
;; | Global ggtags |
;; '---------------'

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1))))

;; ---------
;; hideshow.
;; ---------
(require 'hideshow)
(global-set-key [left-margin down-mouse-1] 'hs-mouse-toggle-hiding)

;; -----------
;; google-this
;; -----------
(require 'google-this)
(google-this-mode 1)


;; **********
;; *        *
;; * Doremi *
;; *        *
;; **********
(setq doremi-up-keys '(up ?\C-p ?k))
(setq doremi-down-keys '(down ?\C-n ?j))


;; *********
;; *       *
;; * Dired *
;; *       *
;; *********

(add-to-list 'load-path "~/.emacs.d/dired-hacks")
(require 'dired+)
(require 'dired-details+)
(require 'dired-efap)
(define-key dired-mode-map [f2] 'dired-efap)
(define-key dired-mode-map [down-mouse-1] 'dired-efap-click)
(setq-default dired-listing-switches "-lGh --group-directories-first")
(defun dired-separate-extensions ()
  (interactive)
  (setq-default dired-listing-switches (concat "-X " dired-listing-switches)))
(setq dired-alt-listing-switches "-alhv")


(require 'dired-imenu)
(add-hook
 'dired-mode-hook
 (lambda ()
   (auto-revert-mode 1)
   ;; use single buffer
   (toggle-diredp-find-file-reuse-dir 1)
   ;; open file in new buffer
   (defalias 'df 'dired-find-file)
   (require 'dired-imenu)
   (require 'dired-open)
   (define-key dired-mode-map (kbd "C-c l") 'dired-open-xdg)
   (defalias 'dx 'dired-open-xdg)))

;; *****************************
;; *                           *
;; * Buffers, Windows & Frames *
;; *                           *
;; *****************************

;; (load "~/.emacs.d/init-popwin.el")

(windmove-default-keybindings 'meta)  ; meta-arrow to move buffers
(global-set-key [C-tab] 'next-multiframe-window)
(global-set-key [C-S-iso-lefttab] 'previous-multiframe-window)

;; -----------
;; buffer-move
;; -----------
(require 'buffer-move)
(global-set-key (kbd "<M-S-up>")     'buf-move-up)
(global-set-key (kbd "<M-S-down>")   'buf-move-down)
(global-set-key (kbd "<M-S-left>")   'buf-move-left)
(global-set-key (kbd "<M-S-right>")  'buf-move-right)
(global-set-key (kbd "M-J")  'buf-move-down)  
(global-set-key (kbd "M-K")  'buf-move-up)
(global-set-key (kbd "M-H")  'buf-move-left)
(global-set-key (kbd "M-L")  'buf-move-right)

;; ---------------
;; transpose-frame
;; ---------------
(require 'transpose-frame)
(global-set-key (kbd "C-x C-/")  'transpose-frame)

;; --------------------
;; function definitions
;; --------------------

(defun win-swap ()
  "Swap windows using buffer-move.el"
  (interactive)
  (cond ((windmove-find-other-window 'right) (buf-move-right))
	((windmove-find-other-window 'left)  (buf-move-left))
	((windmove-find-other-window 'down)  (buf-move-down))
	((windmove-find-other-window 'up)    (buf-move-up)))
  (other-window 1))
(global-set-key (kbd "<S-return>")  'win-swap)

(defun toggle-frame-split ()
"  If the frame is split vertically, split it horizontally or vice versa.
  Assumes that the frame is only split into two."
;; http://www.emacswiki.org/emacs/ToggleWindowSplit
  (interactive)
  (unless (= (length (window-list)) 2) (error "Can only toggle a frame split in two"))
  (let ((split-vertically-p (window-combined-p)))
    (delete-window) ; closes current window
    (if split-vertically-p
        (split-window-horizontally)
      (split-window-vertically)) ; gives us a split with the other window twice
    (switch-to-buffer nil))) ; restore the original window in this part of the frame
(global-set-key "\C-x55" 'toggle-frame-split)
(global-set-key "\C-x\\" 'toggle-frame-split)


;; ********************
;; *                  *
;; * Other Extensions *
;; *                  *
;; ********************

;; --------------
;; CUA rectangle.
;; --------------
;; ;; disable for now - use evil-normal-mode C-v instead
;; (setq cua-enable-cua-keys nil) ;; only for rectangles
;; (cua-mode t)

;; ---------
;; elscreen.
;; ---------
(load "elscreen" t t)

;; F9 creates a new elscreen, shift-F9 kills it
(global-set-key (kbd "C-c t a b e") 'elscreen-create)
(global-set-key (kbd "C-c t a b d") 'elscreen-kill)

;; Windowskey+PgUP/PgDown switches between elscreens
(global-set-key (kbd "C-M-_") 'elscreen-previous)
(global-set-key (kbd "C-M-+") 'elscreen-next)

;; ---------
;; facemenu+
;; ---------
(add-to-list 'load-path "~/.emacs.d/drew-adams")
(require 'highlight)
(require 'facemenu+)

(require 'hideshow-org)
(defalias 'hsorg 'hs-org/minor-mode)

;; -----------
;; iedit-mode.
;; -----------
(setq iedit-toggle-key-default [(meta f3)])
;; (add-to-list 'load-path "~/.emacs.d/iedit")
(require 'iedit)

;; ***************
;; *             *
;; * HELP SYSTEM *
;; *             *
;; ***************
(add-to-list 'load-path "~/.emacs.d/drew-adams")
(require 'help-macro)
(require 'backquote)
(require 'naked nil t) ;; (no error if not found): naked-key-description
(require 'help+)
(require 'help-fns+)
(require 'help-mode+)
;; (global-set-key [f1] 'help-on-click/key)

;; ------------------
;; discover-my-major.
;; ------------------
(require 'discover-my-major)
(define-key help-map "\C-m" 'discover-my-major)


;; **********
;; *        *
;; * SHELLS *
;; *        *
;; **********

;; -----
;; IELM.
;; -----
;; http://nullprogram.com/blog/2010/06/10/
(add-hook 'ielm-mode-hook (lambda () (paredit-mode 1)))
(defadvice ielm-eval-input (after ielm-paredit activate)
  "Begin each IELM prompt with a ParEdit parenthesis pair."
  (paredit-open-round))

;; *************
;; *           *
;; * EVIL-MODE *
;; *           *
;; *************

(defun evil-plugins-refresh-dirs ()
  (interactive)
  (add-subdirs-to-load-path "~/.emacs.d/evil-plugins"))
(evil-plugins-refresh-dirs)

(global-set-key [f9] 'evil-mode)

;; (add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)

;; evil-symbol-word-search
(setq-default evil-symbol-word-search t)
(defun toggle-evil-symbol-word-search ()
  (interactive)
  (setf evil-symbol-word-search (not evil-symbol-word-search)))
(defalias 'evsw 'toggle-evil-symbol-word-search)
(define-key evil-normal-state-map (kbd "C-*") #'toggle-evil-symbol-word-search)

(defalias 'evl 'evil-local-mode)

;; -----------------------
;; KEYBINDING INTERACTIONS
;; -----------------------

(add-to-list 'evil-overriding-maps '(yas-minor-mode-map . insert))
(add-to-list 'evil-overriding-maps '(help-mode-map . normal))

(add-hook 'help-mode-hook 
		  '(lambda () 
			 (define-key evil-motion-state-map (kbd "TAB") #'forward-button)))
;; FIXME: turn off when out of help buffer.

;; Modes starting in Emacs state:
(loop for mode in
      '(
		comint-mode  ;; not working for comint-mode?
		term-mode
		shell-mode
		;; dired-mode
		)
	  do (add-to-list 'evil-emacs-state-modes mode))

;; ----------- 
;; KEYBINDINGS 
;; ----------- 
(load "troyp/evil-utils.el")
;; (evil-define-key 'STATE KEYMAP    ;; define key for STATE in KEYMAP

;; remove C-w C-h binding (steals C-h from help system)
(define-key evil-window-map "\C-h" nil)

;; normal-state
(defun insert-space () (interactive) (insert ? ))
(define-key evil-normal-state-map (kbd "S-SPC") #'insert-space)
(define-key evil-normal-state-map (kbd "TAB") #'indent-for-tab-command)
(define-key evil-normal-state-map (kbd "C-S-d") #'evil-scroll-up)
(define-key evil-normal-state-map (kbd "C-S-o") #'evil-jump-forward)
(define-key evil-normal-state-map (kbd "C-e") #'end-of-line)
;; (define-key evil-normal-state-map (kbd "C-:") #'evil-repeat-find-char-reverse)
;; (define-key evil-normal-state-map (kbd "C-;") #'evil-repeat-find-char-reverse)
(define-key evil-normal-state-map (kbd "[ SPC") #'open-line-above)
(define-key evil-normal-state-map (kbd "] SPC") #'open-line-below)
(define-key evil-normal-state-map (kbd "[ b") #'switch-to-prev-buffer)
(define-key evil-normal-state-map (kbd "] b") #'switch-to-next-buffer)
(define-key evil-normal-state-map (kbd "C-y") nil)
(define-key evil-normal-state-map (kbd "gu") #'evil-upcase)
(define-key evil-normal-state-map (kbd "gU") #'evil-downcase)

;; visual state
(defun insert-space-visual () (interactive) (execute-kbd-macro " ") (evil-visual-restore))
(define-key evil-visual-state-map (kbd "S-SPC") #'insert-space-visual)
;; (define-key evil-visual-state-map [32] #'evil-forward-char-or-extend)
(define-key evil-visual-state-map (kbd "C-SPC") #'evil-forward-char-or-extend)
(define-key evil-visual-state-map (kbd "C-\\") #'shell-command-replace-region)

;; motion state
(define-key evil-motion-state-map (kbd "C-e") #'end-of-line)
;; insert state
(define-key evil-insert-state-map (kbd "C-S-a") #'evil-paste-last-insertion)
(define-key evil-insert-state-map (kbd "C-a") #'beginning-of-line)
(define-key evil-insert-state-map (kbd "C-e") #'end-of-line)
(define-key evil-insert-state-map (kbd "C-S-y") #'evil-copy-from-below)
(define-key evil-insert-state-map (kbd "C-v") #'insert-char)
(define-key evil-insert-state-map (kbd "C-l") #'delete-char)
(define-key evil-insert-state-map (kbd "C-S-l") #'backward-delete-char)
(define-key evil-insert-state-map (kbd "C-k") #'evil-insert-digraph)
(define-key evil-insert-state-map (kbd "C-S-k") #'kill-line)
(define-key evil-insert-state-map (kbd "C-.") 'yas-expand)
(define-key evil-insert-state-map (kbd "C-n") #'next-line)
(define-key evil-insert-state-map (kbd "C-p") #'previous-line)

;; evil-mode mozc-mode keybindings:
;; See mozc section above. Workaround. Allows C-\ to toggle japanese input.
(define-key evil-normal-state-map (kbd "C-\\") #'mozc-mode)
(define-key evil-insert-state-map (kbd "C-\\") #'mozc-mode)

;; key bindings for other extensions
;; (define-key evil-visual-state-map (kbd "C-S-c C-S-c")
;;   (lambda () (interactive) (evil-mode nil) (setq mark-active t) (setq deactivate-mark nil)
;;     (exchange-point-and-mark) (mc/edit-lines)))
(define-key evil-visual-state-map (kbd "C-S-c C-S-c") 'mc/edit-lines)
(define-key evil-emacs-state-map (kbd "C-S-c C-S-c") 'mc/edit-lines)
;; package-menu
(define-key package-menu-mode-map (kbd "/") #'evil-search-forward)
(define-key package-menu-mode-map (kbd "?") #'evil-search-backward)
(define-key package-menu-mode-map (kbd "j") #'evil-next-line)
(define-key package-menu-mode-map (kbd "k") #'evil-previous-line)
(define-key package-menu-mode-map (kbd "<f3>") #'evil-search-forward)
(define-key package-menu-mode-map (kbd "S-<f3>") #'evil-search-backward)


;; ESC quits everything:
(define-key evil-normal-state-map           [escape] 'keyboard-quit)
(define-key evil-visual-state-map           [escape] 'keyboard-quit)
(define-key minibuffer-local-map            [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map         [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map    [escape] 'minibuffer-keyboard-quit)

(evil-add-command-properties 'move-beginning-of-line-or-text :repeat 'ignore)

;; digraphs
(setq evil-digraphs-table-user
      '(
        ((?. ? ) . ?\x2024)    ;; one-dot leader
        ((?. ?/) . ?\x2026)    ;; (horizontal) ellipsis
        ((?. ?-) . ?\x30fb)    ;; CJK middle-dot
        ))
(defalias 'digra 'evil-enter-digraphs)  ;; evil-utils

;; evil-leader
(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "e"      'helm-find-file
  "s"      'load-init-file
  "<SPC>"  [?\C-x ?b return]  ;; switch to last used buffer
  "b"      'switch-to-buffer
  "ci"     'evilnc-comment-or-uncomment-lines
  "cl"     'evilnc-quick-comment-or-uncomment-to-the-line
  "cc"     'evilnc-copy-and-comment-lines
  "cp"     'evilnc-comment-or-uncomment-paragraphs
  "cr"     'comment-or-uncomment-region
  "cv"     'evilnc-toggle-invert-comment-line-by-line
  ","      'evilnc-comment-operator
  "k"      'kill-buffer
  "i"      'open-init-file
  "v"      'eval-region
  "V"      'eval-buffer
  "a"      'ace-jump-word-mode
  "f"      'ace-jump-char-mode
  "g"      'ace-jump-line-mode
  "h"      'extra-help-map                 ;; (PREFIX)
  "l"      'helm-mini
  "m"      'mode-ring-prefix-key-map       ;; (PREFIX)
  "n"      'new-file
  "o"      'find-file
  "1"      'delete-other-windows
  "2"      'split-window-below
  "3"      'split-window-right
  "4"      'ctl-x-4-prefix                  ;; (PREFIX) other window commands
  "0"      'delete-window
  "["      'kmacro-start-macro-or-insert-counter
  "]"      'kmacro-end-or-call-macro
  "<f2>"   '2C-command                      ;; (PREFIX)
  "<f4>"   'delete-other-window             ;; (PREFIX)
  ">"      'evil-numbers/inc-at-pt
  "<"      'evil-numbers/dec-at-pt
  "."      'find-tag
  "'"      '(lambda (&optional arg) "Quote surrounding WORD."
              (interactive "p") (kmacro-exec-ring-item (quote ("viWs\"" 0 "%d")) arg))
  "\""     '(lambda (&optional arg) "Quote surrounding word."
              (interactive "p") (kmacro-exec-ring-item (quote ("viws\"" 0 "%d")) arg))
  "\\"     'quick-pcre-align
  "|"      'quick-pcre-align-repeat
  ";"      'asciiheadings-prefix-key-map     ;; (PREFIX)
  )

;; -------
;; PLUGINS
;; -------

;; evil-exchange (vim-exchange)
(require 'evil-exchange)
;; (setq evil-exchange-key (kbd "zx"))
(evil-exchange-install)

;; evil-jumper
(require 'evil-jumper)

;; evil-nerd-commenter
(require 'evil-nerd-commenter)
;; (setq evilnc-hotkey-comment-operator ",,")
;; (evilnc-default-hotkeys)
(global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)
(global-set-key (kbd "C-M-;") 'comment-dwim)

;; evil-numbers
(require 'evil-numbers)
;; also use LEADER-> and LEADER-< in normal-state
(global-set-key (kbd "C-c +") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-c =") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt)

;; evil-matchit - installed via package manager
(require 'evil-matchit)
(global-evil-matchit-mode 1)

;; evil-snipe
(require 'evil-snipe)
(evil-snipe-mode 1)

(defun evil-snipe-multi-bracket ()
  "Use [ for matching [,{,( and ] for ],},)."
  (interactive)
  ;; ?[ breaks autoindent
  (evil-snipe-add-alias (string-to-char "[") "[[{(]")
  (evil-snipe-add-alias (string-to-char "]") "[]})]"))
(add-hook 'emacs-lisp-mode-hook 'evil-snipe-multi-bracket)

;; ;; evil-snipe-override-mode: replace evil-mode's f/F/t/T/;/, with snipe
(defun evil-snipe-toggle-override-mode ()
  (interactive)
  (evil-snipe-override-mode
   (if (evil-snipe-override-mode) 0 1)))
;; (evil-snipe-override-mode 1)
;; (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)

(setf evil-snipe-scope 'whole-visible)
(setf evil-snipe-repeat-scope 'whole-visible)
(evil-define-key 'visual evil-snipe-mode-map "z" 'evil-snipe-s)
(evil-define-key 'visual evil-snipe-mode-map "Z" 'evil-snipe-S)
(evil-define-key 'motion evil-snipe-mode-map "s" 'evil-snipe-s)
(evil-define-key 'motion evil-snipe-mode-map "S" 'evil-snipe-S)
(evil-define-key 'operator evil-snipe-mode-map "z" 'evil-snipe-s)
(evil-define-key 'operator evil-snipe-mode-map "Z" 'evil-snipe-S)
(evil-define-key 'operator evil-snipe-mode-map "x" 'evil-snipe-x)
(evil-define-key 'operator evil-snipe-mode-map "X" 'evil-snipe-X)
;; bindings take eff ect when evil-snipe-override-mode is on.
(evil-define-key 'motion evil-snipe-override-mode-map "f" 'evil-snipe-f)
(evil-define-key 'motion evil-snipe-override-mode-map "F" 'evil-snipe-F)
(evil-define-key 'motion evil-snipe-override-mode-map "t" 'evil-snipe-t)
(evil-define-key 'motion evil-snipe-override-mode-map "T" 'evil-snipe-T)
(when evil-snipe-override-evil-repeat-keys
  (evil-define-key 'motion map ";" 'evil-snipe-repeat)
  (evil-define-key 'motion map "," 'evil-snipe-repeat-reverse))

;; evil-surround
(require 'surround)
(global-surround-mode 1)

;; ;; evil-tabs
;; (require 'evil-tabs)
;; (global-evil-tabs-mode t)
;; (defun tabs ()
;;   (interactive)
;;   (elscreen-toggle-display-tab))

;; evil-visualstar
(require 'evil-visualstar)

;; **********************
;; *                    *
;; * VIMRC-GENERIC-MODE *
;; *                    *
;; **********************


;; ;; defined by Gilles in:
;; ;; http://stackoverflow.com/questions/4236808/syntax-highlight-a-vimrc-file-in-emacs
;; (define-generic-mode 'vimrc-generic-mode
;;   '()
;;   '()
;;   '(("^[\t ]*:?\\(!\\|ab\\|map\\|unmap\\)[^\r\n\"]*\"[^\r\n\"]*\\(\"[^\r\n\"]*\"[^\r\n\"]*\\)*$"
;;      (0 font-lock-warning-face))
;;     ("\\(^\\|[\t ]\\)\\(\".*\\)$"
;;      (2 font-lock-comment-face))
;;     ("\"\\([^\n\r\"\\]\\|\\.\\)*\""
;;      (0 font-lock-string-face)))
;;   '("/vimrc\\'" "\\.vim\\(rc\\)?\\'")
;;   '((lambda ()
;;       (modify-syntax-entry ?\" ".")))
;;   "Generic mode for Vim configuration files.")

(add-to-list 'auto-mode-alist '("\\.vim\\(rc\\)?-?\\w*$" . vimrc-mode))
(add-to-list 'auto-mode-alist '("\\.pentadactylrc-?\\w*$" . vimrc-mode))

;; ************
;; *          *
;; * Dotfiles *
;; *          *
;; ************

(add-to-list 'auto-mode-alist '("\\.keynavrc$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.bash[[:alnum:]-\.]*$" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.gitconfig$" . conf-mode))



;; **********
;; *        *
;; * AUCTEX *
;; *        *
;; **********

(load "auctex.el" nil t t)
;; (load "preview-latex.el" nil t t)
(load "preview.el" nil t t)
(require 'auto-complete-auctex)
(setq LaTeX-indent-level 4)
;; predictive-mode (for latex "intellisense")
(add-to-list 'load-path "/home/troy/.emacs.d/predictive")
(add-to-list 'load-path "/home/troy/.emacs.d/predictive/misc")
(add-to-list 'load-path "/home/troy/.emacs.d/predictive/texinfo")
(add-to-list 'load-path "/home/troy/.emacs.d/predictive/latex")
(add-to-list 'load-path "/home/troy/.emacs.d/predictive/html")
(autoload 'predictive-mode "predictive" "predictive" t)
(set-default 'predictive-auto-add-to-dict t)
(setq
 ;; predictive-main-dict 'rpg-dictionary
 predictive-auto-learn t
 ;; predictive-add-to-dict-ask nil
 predictive-use-auto-learn-cache nil
 predictive-which-dict t)


;; ****************
;; *              *
;; *  Emacs-Eclim *
;; *              *
;; ****************

(add-to-list 'load-path "/home/troy/.emacs.d/emacs-eclim")
(require 'eclim)
(global-eclim-mode)
(require 'eclimd)
(setq eclim-executable "/opt/eclipse/plugins/org.eclim_2.3.2/bin/eclim")
(setq eclimd-executable "/opt/eclipse/plugins/org.eclim_2.3.2/bin/eclimd")
(setq eclimd-default-workspace "/home/troy/code/eclipse")


;;-----------------------------------------------------------------------------

;; *************************
;; *                       *
;; * PROGRAMMING LANGUAGES *
;; *                       *
;; *************************

;; *********
;; *       *
;; * C/C++ *
;; *       *
;; *********

;; (setq c-basic-offset 4)
(c-add-style "troy"
	     '("stroustrup"
	       (c-basic-offset . 4)
	       (indent-tabs-mode . nil)
	       (c-offsets-alist
		(case-label . 1)
		(statement-case-intro . 3)
		(access-label . -3))))
(setq c-default-style '((c-mode . "troy")
			(java-mode . "java")
			(awk-mode . "awk")
			(c++-mode . "troy")
			(other . "gnu")))
(load "c++-fontlock-custom")
(load "c++-fontlock-fix-enum-class.el")
(load "troyp/c++-utils.el")

;; make _ part of words
(defun c-mode-include-underscore-in-words ()
  (interactive)
  (modify-syntax-entry ?_ "w" c-mode-syntax-table))
(add-hook 'c-mode-common-hook 'c-mode-include-underscore-in-words)


;; EMACS-CLANG-COMPLETE-ASYNC

;; (add-to-list 'load-path "~/.emacs.d/emacs-clang-complete-async")
;; (require 'auto-complete-config)
;; (require 'auto-complete-clang-async)

;; (defun ac-cc-mode-setup ()
;;   (setq ac-clang-complete-executable "~/.emacs.d/emacs-clang-complete-async/clang-complete")
;;   (setq ac-sources '(ac-source-clang-async))
;;   (ac-clang-launch-completion-process)
;; )

;; (defun my-ac-config ()
;;   (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
;;   ;; (add-hook 'auto-complete-mode-hook 'ac-common-setup)
;;   (add-hook 'auto-complete-mode-hook 'ac-cc-mode-setup)
;;   (global-auto-complete-mode t))

;; (my-ac-config)

(defun comment-use-line-comments ()
  (interactive)
  (setq-local comment-start "//")
  (setq-local comment-end "")
  (setq-local comment-type "line")
  (message "Using line comments"))

(defun comment-use-block-comments ()
  (interactive)
  (setq-local comment-start "/*")
  (setq-local comment-end "*/")
  (setq-local comment-type "block")
  (message "Using block comments"))

(defun comment-toggle-line-block ()
  (interactive)
  (if (and (boundp 'comment-type)
           (string= comment-type "line"))
      (comment-use-block-comments)
    (comment-use-line-comments)))

;; *******
;; *     *
;; *  D  *
;; *     *
;; *******

(autoload 'd-mode "d-mode" "Major mode for editing D code." t)
(add-to-list 'auto-mode-alist '("\\.d[i]?\\'" . d-mode))


;; **********
;; *        *
;; * Erlang *
;; *        *
;; **********

;; (setq load-path (cons (car (file-expand-wildcards "/usr/lib/erlang/lib/tools-*/emacs"))
;; 		      load-path))
;; (setq erlang-root-dir "/usr/lib/erlang")
;; (setq exec-path (cons "/usr/lib/erlang/bin" exec-path))
;; (require 'erlang-start)

;; (setq inferior-erlang-machine-options '("-sname" "emacs"))
;; (add-to-list 'load-path "~/.emacs.d/distel/elisp")
;; (require 'distel)
;; (distel-setup)


;; **********
;; *        *
;; * Factor *
;; *        *
;; **********

(defun run-fuel ()
  (interactive)
  (load-file "/opt/factor/misc/fuel/fu.el")
  (run-factor))
(add-hook 'factor-mode-hook (lambda () (variable-pitch-mode t)))


;; ***********
;; *         *
;; * Haskell *
;; *         *
;; ***********

(add-hook 'haskell-mode-hook 'haskell-indent-mode)
(add-hook 'haskell-mode-hook 'haskell-doc-mode)
(add-hook 'haskell-mode-hook 'inf-haskell-mode)
(define-key haskell-mode-map (kbd "C-c .") 'hoogle-lookup)
(defun hoogle-lookup (s)
  (interactive "sHoogle: ")
  (message
   (shell-command-to-string
	(concat "/home/troy/.cabal/bin/hoogle " s))))



;; ;; Scion.
;; ;; ======
;; (add-to-list 'load-path "~/.emacs.d/plugins/scion-master/emacs")
;; (require 'scion)
;; (setq scion-program "~/.cabal/bin/scion-server")
;; (defun my-haskell-hook ()
;;   ;; Whenever we open a file in Haskell mode, also activate Scion
;;   (scion-mode 1)
;;   ;; Whenever a file is saved, immediately type check it and
;;   ;; highlight errors/warnings in the source.
;;   (scion-flycheck-on-save 1))
;; (add-hook 'haskell-mode-hook 'my-haskell-hook)
;; ;; Use ido-mode completion (matches anywhere, not just beginning)
;; ;; WARNING: This causes some versions of Emacs to fail so badly
;; ;; that Emacs needs to be restarted.
;; (setq scion-completing-read-function 'ido-completing-read)


;; *******
;; *     *
;; *  J  *
;; *     *
;; *******

(autoload 'j-mode "j-mode.el"  "Major mode for J." t)
(autoload 'j-shell "j-mode.el" "Run J from emacs." t)
(setq auto-mode-alist
      (cons '("\\.ij[rstp]" . j-mode) auto-mode-alist))
(setq j-path "/opt/j64-701/")
;; if don't need plotting/graphics...
(setq j-command "bin/jconsole")


;; **************
;; *            *
;; * Javascript *
;; *            *
;; **************

;; add js2-minor-mode to js-mode
(add-hook 'js-mode-hook
	  '(lambda ()
	     (js2-minor-mode)
	     (define-key js2-mode-map (kbd "C-c C-u") 'js2-cancel-error-face)
		 (setq indent-tabs-mode nil)))
(add-hook 'js2-minor-mode-hook 'ac-js2-mode)

;; default js mode: js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsm$" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(add-hook 'js2-mode-hook 'ac-js2-mode)
(setq js2-highlight-level 3)
(defun js2-cancel-error-face ()
  (interactive)
  (face-remap-add-relative 'js2-error nil))


;; ************
;; *          *
;; * Markdown *
;; *          *
;; ************
;; ------------------------
;;   Polymode - RMarkdown
;; ------------------------
(require 'poly-markdown)
(defalias 'rmd-mode 'poly-markdown+r-mode)
;;; MARKDOWN
(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))

;;; R modes
(add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))


;; **********
;; *        *
;; * Octave *
;; *        *
;; **********

(autoload 'octave-mode "octave-mod" nil t)
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))
(add-hook 'octave-mode-hook
          (lambda ()
            (abbrev-mode 1)
            (auto-fill-mode 1)
            (if (eq window-system 'x)
		(font-lock-mode 1))))


;; ********
;; *      *
;; *  Oz  *
;; *      *
;; ********

;; (or (getenv "OZHOME") (setenv "OZHOME" "/opt/mozart2"))
(or (getenv "OZHOME") (setenv "OZHOME" "/usr"))
(add-to-list 'load-path (concat (getenv "OZHOME") "/share/mozart/elisp"))
(require 'oz)
(add-to-list 'auto-mode-alist '("\\.oz\\'" . oz-mode))
(add-to-list 'auto-mode-alist '("\\.ozg\\'" . oz-gump-mode))
(autoload 'run-oz "oz" "" t)
(autoload 'oz-mode "oz" "" t)
(autoload 'oz-gump-mode "oz" "" t)
(autoload 'oz-new-buffer "oz" "" t)
(define-key oz-mode-map (kbd "C-c C-c") 'oz-feed-line)
(define-key oz-mode-map (kbd "C-c C-p") 'oz-feed-paragraph)
(define-key oz-mode-map (kbd "C-c C-r") 'oz-feed-region)
(define-key oz-mode-map (kbd "C-c C-b") 'oz-feed-buffer)
(define-key oz-mode-map (kbd "M-p") 'evil-scroll-line-up)
(define-key oz-mode-map (kbd "M-n") 'evil-scroll-line-down)


;; ********
;; *      *
;; * Perl *
;; *      *
;; ********

(add-to-list 'load-path "/home/troy/.emacs.d/Emacs-PDE-0.2.16/lisp/")
(load "pde-load")
(global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)
(defun perl-completion-on ()
  (require 'perl-completion)
  (perl-completion-mode t))
(add-hook 'cperl-mode-hook 'perl-completion-on)


;; **********
;; *        *
;; * PROLOG *
;; *        *
;; **********

(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)
(setq prolog-system 'swi)
(setq auto-mode-alist
      (append
       '(
		 ;; ("\\.pl$" . prolog-mode)
		 ("\\.m$" . mercury-mode)
		 ("\\.prolog$" . prolog-mode)
		 ("\\.pro$" . prolog-mode))
       auto-mode-alist))


;; **********
;; *        *
;; * Python *
;; *        *
;; **********

;; ----------------
;; emacs for python
;; ----------------
(defun epy-load () (interactive) (load-file "/home/troy/.emacs.d/emacs-for-python/epy-init.el"))
;(epy-django-snippets)
;(epy-setup-ipython)
;(global-hl-line-mode t) ;; To enable
;(set-face-background 'hl-line "black") ;; change with the color that you like
 ;(require 'highlight-indentation)
 ;(add-hook 'python-mode-hook 'highlight-indentation)

(add-to-list 'load-path "/home/troy/.emacs.d/python-mode.el/")
(setq py-install-directory "/home/troy/.emacs.d/python-mode.el/")
(require 'python-mode)
(setq py-shell-name "ipython")
(setq py-shell-name "/usr/bin/ipython")
(defun py-splith ()
  (interactive)
  (custom-set-variables
   '(py-split-windows-on-execute-function (quote split-window-horizontally))))
(defun py-splitv ()
  (interactive)
  (custom-set-variables
   '(py-split-windows-on-execute-function (quote split-window-vertically))))

;; Ein.
(require 'ein)
;; start server with "ipython notebook --pylab=inline"
;; open notebook with "M-x ein:notebooklist-open" or "M-x ein:notebooklist-new"

;; Jedi.
(defun my-jedi-mode ()
  ;; (global-set-key [C-tab] 'next-multiframe-window)
  ;; (global-set-key (kbd "C-`") 'jedi:complete)
  (jedi:setup)
  (define-key jedi-mode-map (kbd "C-`") 'jedi:complete)
  (define-key jedi-mode-map [C-tab] 'next-multiframe-window))
(add-hook 'python-mode-hook 'my-jedi-mode)
; (add-hook 'python-mode-hook 'jedi:ac-setup)  ; autocomplete only
(setq jedi:setup-keys t)  ; keybindings - must be set *before* jedi.el loaded
(setq jedi:complete-on-dot t)


;; *******
;; *     *
;; *  R  *
;; *     *
;; *******

;; -------
;;   ESS
;; -------
;; C-c C-e C-t: tags for directory
;; C-c C-d C-e: describe object at point
(require 'ess)
(setq ess-eval-visibly nil)
(setq ess-ask-for-ess-directory nil)
(setq ess-local-process-name "R")
;; (setq ansi-color-for-comint-mode 'filter)
;; (setq comint-prompt-read-only t)
;; (setq comint-scroll-to-bottom-on-input t)
;; (setq comint-scroll-to-bottom-on-output t)
;; (setq comint-move-point-for-output t)
(require 'r-utils)
(add-hook 'ess-mode-hook
	  '(lambda()
	     (local-set-key [(shift return)] 'ess-shift-enter)))
;; ess-autoyas: use yas for function arguments
(require 'r-autoyas)
(add-hook 'ess-mode-hook 'r-autoyas-ess-activate)
;; ess-auto-complete
(defalias 'ess-auto-complete-start 'ess-ac-start)
(setq ess-use-auto-complete 'script-only)
(define-key ac-completing-map "\M-?" 'ac-complete)
(define-key ac-completing-map "\M-n" nil) ;; was ac-next
(define-key ac-completing-map "\M-p" nil) ;; was ac-previous
(define-key ac-completing-map "\C-s" 'ac-next)
(define-key ac-completing-map "\C-r" 'ac-previous)
;; speedbar
(add-hook 'speedbar-mode-hook '(lambda () (speedbar-add-supported-extension ".R")))

;; RMarkdown (polymode): see Markdown.

;; ********
;; *      *
;; * SAGE *
;; *      *
;; ********
;; (add-to-list 'load-path "/usr/lib/sagemath/local/share/emacs/site-lisp/sage-mode/")
;; (require 'sage "sage")
;; (setq sage-command "/usr/lib/sagemath/sage")


;; *********
;; *       *
;; * Scala *
;; *       *
;; *********

;; Scala-mode.
;; (add-to-list 'load-path "~/.emacs.d/scala-mode2-master")
(require 'scala-mode2)
(add-to-list 'exec-path "/opt/scala/bin/")

;; Ensime.
(add-to-list 'load-path "~/.emacs.d/ensime-master/src/main/elisp/")
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;; Scala Keybindings.
(add-hook 'scala-mode-hook '(lambda ()
  (local-set-key (kbd "RET") '(lambda ()
    (interactive)
    (newline-and-indent)
    (scala-indent:insert-asterisk-on-multiline-comment)))
  (local-set-key (kbd "M-RET") 'join-line)
  (local-set-key (kbd "<backtab>") 'scala-indent:indent-with-reluctant-strategy)))

;; SBT.
(add-to-list 'exec-path "/opt/sbt/bin/")


;; **********
;; *        *
;; * Scheme *
;; *        *
;; **********

(add-hook 'scheme-mode-hook 'my-scheme-mode-hook)
(defun my-scheme-mode-hook ()
  (setq font-lock-defaults
	'((scheme-font-lock-keywords
	   scheme-font-lock-keywords-1
	   my-scheme-font-lock-keywords)
	  nil t (("+-*/.<>=!?$%_&~^:" . "w")) beginning-of-defun
	  (font-lock-mark-block-function . mark-defun)))
  (setq my-scheme-font-lock-keywords
	(append scheme-font-lock-keywords-2
		(eval-when-compile
		  (list
		   (regexp-opt '("compensate" "when") t)
		   ;;This must come before the errors specification, or
		   ;;"misc-error" will not be colored correctly.
		   (cons (regexp-opt '("wrong-type-arg" "misc-error"
				       "out-of-range" "system-error") t)
			 'font-lock-constant-face)
		   (cons (regexp-opt '("scm-error" "error"
				       "false-if-exception") t)
			 'font-lock-warning-face))))))
(defconst my-scheme-font-lock-keywords
  '()
  "Custom highlighting in Scheme modes.")


;; *********
;; *       *
;; * Shell *
;; *       *
;; *********

;; shell-script
(add-to-list 'auto-mode-alist '("\\.bash" . sh-mode))

;; defaults
(setq-default sh-indent-for-then 0)
(setq-default sh-indent-for-do 0)
(setq-default sh-indent-after-do '+)
(setq-default sh-indent-for-case-label '*)
(setq-default sh-indent-for-case-alt '+)
(setq-default sh-indent-comment t)

;; ANSI-TERM-MODE.
(setq term-bind-key-alist-defaults
      '(("C-c C-c" . term-interrupt-subjob)
	("C-p" . previous-line)
	("C-n" . next-line)
	("C-s" . isearch-forward)
	("C-r" . isearch-backward)
	("C-m" . term-send-raw)
	("M-f" . term-send-forward-word)
	("M-b" . term-send-backward-word)
	("M-o" . term-send-backspace)
	("M-p" . term-send-up)
	("M-n" . term-send-down)
	("M-M" . term-send-forward-kill-word)
	("M-N" . term-send-backward-kill-word)
	("M-r" . term-send-reverse-search-history)
	("M-," . term-send-input)
	("M-." . comint-dynamic-complete)))
(setq my-term-bind-key-alist
      '(("C-c C-j"   .  term-line-mode)
	("C-c C-k"   .  term-char-mode)
	;; ("M-DEL"     .  term-send-backward-kill-word)
	;; ("M-d"	.  term-send-forward-kill-word)
	;; ("<C-left>"	.  term-send-backward-word)
	;; ("<C-right>" .  term-send-forward-word)
	;; ("C-r"	.  term-send-reverse-search-history)
	;; ("M-p"	.  term-send-raw-meta)
	;; ("M-y"	.  term-send-raw-meta)
	;; ("C-y"	.  term-send-raw)
	))
(defun my-ansi-mode-hook ()
  (setq term-bind-key-alist
	(append term-bind-key-alist-defaults
		my-term-bind-key-alist)))
(add-hook 'term-mode-hook 'my-ansi-mode-hook)

(require 'multi-term)
(global-set-key (kbd "<f5>") 'multi-term)
(global-set-key (kbd "<C-next>") 'multi-term-next)
(global-set-key (kbd "<C-prior>") 'multi-term-prev)
(setq multi-term-program "/bin/bash")


;; *******
;; *     *
;; * SML *
;; *     *
;; *******


;; ***********
;; *         *
;; * WEB DEV *
;; *         *
;; ***********
(skewer-setup)

(defun htmlentify (start end)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (beginning-of-buffer)
      (replace-string "&" "&amp;")
      (beginning-of-buffer)
      (replace-string "<" "&lt;")
      (beginning-of-buffer)
      (replace-string ">" "&gt;"))))

;;---------------------------------------------------------------------------

;; *********************
;; *                   *
;; * OTHER MAJOR MODES *
;; *                   *
;; *********************

(require 'ass-mode)

;; -----------------------------------------------------------------------------

;; **********************
;; *                    *
;; * GLOBAL FONT HEIGHT *
;; *                    *
;; **********************
;; note: for current buffer font-height only, use text-scale-adjust(C-x C-=, C-x C--)

(defun get-font-size ()
  "Returns the current default font size in decipoints"
  (interactive)
  (let ((font-height (face-attribute 'default :height nil 'default)))
    (message (number-to-string font-height))
    font-height))
  
(defun get-default-font-size ()
  "Returns the current default font size in decipoints"
  (interactive)
  (let ((font-height (face-attribute 'default :height t 'default)))
    (message (number-to-string font-height))
    font-height))

(defun set-font-size (FONT-HEIGHT)
  "Sets the current default font size to FONT-HEIGHT in decipoints (defaults to 110 = 11pt)"
  (interactive "NNew Font Height in pts: ")
  (set-face-attribute 'default nil :height FONT-HEIGHT)
  FONT-HEIGHT)

(defun zoom-in (INC-HEIGHT)
  "Increase font size by INC-HEIGHT decipoints (default 5 = 0.5 points)"
  (interactive "p")
  ;; default increment: 5 decipoints
  (if (= INC-HEIGHT 1)
      (setq INC-HEIGHT 5))
 (let ((font-height (+ (get-font-size)
			INC-HEIGHT)))
    (set-font-size font-height)
    (message (number-to-string font-height))))

(defun zoom-out (DEC-HEIGHT)
  "Increase font size by DEC-HEIGHT decipoints (default 5 = 0.5pts)"
  (interactive "p")
  ;; default increment: 5 decipoints
  (if (= DEC-HEIGHT 1)
      (setq DEC-HEIGHT 5))
  (let ((font-height (- (get-font-size)
			DEC-HEIGHT)))
    (set-font-size font-height)
    (message (number-to-string font-height))))

;; ------------------------------------------------------------------------
;; SET PATH CORRECTLY.

(defun sync-path ()
  (interactive)
  (let ((sh-path (split-string-and-unquote
		  (shell-command-to-string
		   ". ~/.bashrc &> /dev/null; echo -n $PATH 2> /dev/null")
		  ":")))
    (setq exec-path (remove-dups (append exec-path sh-path)))))

;; ------------------------------------------------------------------------
;; UTILITY FUNCTIONS.
;; (defun bprint (x)    // use "insert"
;;   (print x (current-buffer)))

(defun kill-and-close-window ()
  (interactive)
  (kill-buffer)
  (delete-window))
(global-set-key "\C-xc" 'kill-and-close-window)

(defun kill-current-buffer ()
  (interactive)
  (kill-buffer nil))

;; https://github.com/davvil/.emacs.d/blob/master/init.el
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(defun load-init-file ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))
(defun open-init-file ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))


;; -----------------------------------------------------------------------------

;; *********
;; *       *
;; * MOUSE *
;; *       *
;; *********

;; (load "~/.emacs.d/init-mouse.el")


;; ******************************
;; *                            *
;; * MODE-SPECIFIC KEY BINDINGS *
;; *                            *
;; ******************************

(defun insert-backquote ()
  (interactive)
  (insert-char ?`))

(define-key-multi-modes (kbd "C-'") 'insert-backquote
  '(shell-mode-map
    haskell-mode-map
    markdown-mode-map))

;; Insert Continuation Symbol.
;; TODO: redefine column number
(defun insert-line-continuation ()
  "insert \ at column 80"
  (interactive)
  (end-of-line)
  (insert-char 32 (- 79 (current-column)))
  (insert-char '?\\' 1))
(define-key-multi-modes "\C-x\\" 'insert-line-continuation
  '(python-mode-map))

(add-hook
 'sh-mode-hook
 (lambda nil
   (define-key sh-mode-map "\C-x\\" 'insert-continuation)
   (define-key sh-mode-map (kbd "C-'") 'insert-backquote)))

;; ***********
;; *         *
;; * KEYMAPS *
;; *         *
;; ***********

(defun extra-help-apropos-all-variables (s)
  (interactive "sVariable: ")
  (apropos-variable s t))
(defun extra-help-apropos-user-variables (s)
  (interactive "sVariable: ")
  (apropos-variable s))

;; define key sequence relative to prefix key
(define-prefix-command 'extra-help-map)
(define-key 'extra-help-map "va" 'extra-help-apropos-all-variables)
(define-key 'extra-help-map "vu" 'extra-help-apropos-user-variables)

;; define key sequence explicitly
(define-prefix-command 'menu-key-map)
(global-set-key (kbd "<menu>") 'menu-key-map)
(global-set-key (kbd "<menu>1") 'linum-mode)


;; ****************
;; *              *
;; * KEY BINDINGS *
;; *              *
;; ****************

(defun get-face (&optional pos)
  (interactive)
  (message "face: %s" (get-char-property (or pos (point)) 'face)))
(global-set-key [f8] 'get-face)
(global-set-key [\M-f8] 'what-cursor-position)
(global-set-key [\C-f8] 'describe-char)
(global-set-key [\S-f8] 'palette-foreground-at-point)


(global-set-key "\C-a" 'move-beginning-of-line-or-text)
(global-set-key (kbd "C-S-l") #'backward-delete-char)
(global-set-key (kbd "M-<backspace>") 'backward-kill-word)
(global-set-key (kbd "C-SPC") 'cua-set-mark)

(global-set-key [\C-\S-up] 'move-text-up)
(global-set-key [\C-\S-down] 'move-text-down)
(global-set-key [\C-\S-\delete] 'remove-current-line)
;; (global-set-key [\C-\S-\left] 'cut-current-line)
;; (global-set-key [\C-\S-\right] 'copy-current-line)
(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)

(global-set-key [kp-home]  'beginning-of-buffer) ; [Home]
(global-set-key [home]     'beginning-of-buffer) ; [Home]
(global-set-key [kp-end]   'end-of-buffer)       ; [End]
(global-set-key [end]      'end-of-buffer)       ; [End]
(global-set-key [\M-f12]   'shell-pop)

;(global-set-key (kbd "M-;") 'comment-dwim) ;; replaced by evil-nc- function
;(global-set-key "\M-f" 'forward-sexp)
;(global-set-key "\M-b" 'backward-sexp)
(global-set-key "\M-sq" 'insert-char)
(global-set-key "\C-z" 'undo)    ; [Undo]
;; (global-set-key "\M-1" 'goto-line)  ;; use "M-g g" or "M-g M-g"
(global-set-key "\M-c" 'capitalize-word)
(global-set-key (kbd "C-x C-9") 'variable-pitch-mode)
;(global-set-key "\C-h" 'delete-backward-char)
;(global-set-key "\C-?" 'help-command)
(global-set-key "\M-o" 'occur)
(global-set-key (kbd "C-x SPC") 'just-one-space)
(global-set-key [M-f1] 'apropos-follow)

(global-set-key (kbd "M-DEL") 'kill-current-buffer)
;; change C-x - from 'shrink-window-if-larger-than-buffer to 'fit-window-to-buffer
(global-set-key (kbd "\C-x -") 'fit-window-to-buffer)

;; Scrolling Up & Down.
(global-set-key (kbd "M-n") 'evil-scroll-line-down)
(global-set-key (kbd "M-p") 'evil-scroll-line-up)
(global-set-key (kbd "C-S-n")
		(lambda () (interactive) (scroll-other-window-down 1)))
(global-set-key (kbd "C-S-p")
		(lambda () (interactive) (scroll-other-window-down -1)))

;; KEYBOARD MACROS.
;; 
(fset 'switch-to-most-recent-buffer [?\C-x ?b return])


;; *************
;; *           *
;; * FUNCTIONS *
;; *           *
;; *************

(defun auto-complete-mode-off ()
  (interactive)
  (auto-complete-mode 0))

;; defiwrap DEFINITIONS (utils.el)
(defiwrap fnks '(kill-new buffer-file-name))

;; KEYBOARD MACROS.
(let ((kmacro-init (concat user-init-file "-kmacros")))
  (if (file-exists-p kmacro-init)
      (load  kmacro-init)))
;; save-macro: from emacs-wiki, modified to use .emacs-kmacros
(defun save-macro (name)
  "save a macro. Take a name as argument
     and save the last defined macro under
     this name in a .emacs-kmacros file"
  (interactive "SName of the macro :")  ; ask for the name of the macro
  (kmacro-name-last-macro name)         ; use this name for the macro
  (find-file (concat user-init-file
		     "-kmacros"))       ; open ~/.emacs or other user init file
  (goto-char (point-max))               ; go to the end of the .emacs
  (newline)                             ; insert a newline
  (insert-kbd-macro name)               ; copy the macro
  (newline)                             ; insert a newline
  (switch-to-buffer nil))               ; return to the initial buffer

(defun new-file (filename &optional wildcards)
  "Open new file. Derived from Emacs source. GPL3."
  (interactive
   (find-file-read-args
    "Find file: "
    (confirm-nonexistent-file-or-buffer)))
  (let ((value (find-file-noselect filename nil nil wildcards)))
    (if (listp value)
        (mapcar 'switch-to-buffer (nreverse value))
      (switch-to-buffer value))))
 

;; ***********
;; *         *
;; * ALIASES *
;; *         *
;; ***********

(defalias 'reyas 'yas/reload-all)
(defalias 'boxcom 'box-heading-comment)
(defalias 'reccom 'rect-heading-comment)
(defalias 'srecom 'short-rect-heading-comment)
(defalias 'arv 'auto-revert-mode)
(defalias 'revb 'revert-buffer)
(defalias 'diffb 'diff-buffer-with-file)
(defalias 'init 'open-init-file)
(defalias 'sim 'set-input-method)  ;; bound to C-\
(defalias 'repl 'ielm)
(defalias 'lim 'lisp-interaction-mode)
(defalias 'el 'emacs-lisp-mode)
(defalias 'ppr 'cl-prettyprint)
(defalias 'chmodx 'make-executable)
(defalias 'unset 'makunbound)
(defalias 'unfset 'fmakunbound)
(defalias 'vll 'visual-line-mode)
(defalias 'undefun 'fmakunbound)
(defalias 'acoff 'auto-complete-mode-off)
(defalias 'ali 'quick-pcre-align-repeat)

;; FINAL
(setq skeleton-pair nil)
(put 'upcase-region 'disabled nil)
(cua-mode 0)
(put 'narrow-to-region 'disabled nil)
