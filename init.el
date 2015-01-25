(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-brace-indent-level 4)
 '(ansi-color-names-vector ["#3f3f3f" "#cc9393" "#7f9f7f" "#f0dfaf" "#8cd0d3" "#dc8cc3" "#93e0e3" "#dcdccc"])
 '(browse-url-browser-function (quote browse-url-generic))
 '(browse-url-generic-program "/usr/bin/google-chrome")
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(custom-safe-themes (quote ("6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "b7553781f4a831d5af6545f7a5967eb002c8daeee688c5cbf33bf27936ec18b3" "60483aeff1d740449af9a5346f86a6fbe0f8e8f07379f916d9217235bb681f9c" "e992575f7c09459bfc190e6776b8f5f96964023e98267a87fb3094e7c9686776" "cf2bb5e8046ca363183c87e8d33932f2a76a3d705b9db2721631777bbce92968" "4870e6cb6f0a70c14ee73db30b69a8a1f08d6ec9a689c366e88636fb81e8022d" "78b1c94c1298bbe80ae7f49286e720be25665dca4b89aea16c60dacccfbb0bca" "e439d894bf9406baf73056cf7e3c913ee5c794b6adadbbb9f614aebed0fd9ce7" "9117c98819cfdeb59780cb43e5d360ff8a5964d7dd9783b01708bda83098b9fd" default)))
 '(ecb-options-version "2.40")
 '(eclim-eclipse-dirs (quote ("/opt/eclipse")))
 '(ensime-default-server-cmd "/opt/scala/ensime/etc/scripts/server")
 '(fci-rule-color "#383838")
 '(geiser-racket-binary "/opt/racket/bin/racket")
 '(inferior-octave-program "/usr/bin/octave")
 '(j-dictionary-url "/opt/j64-701/help/dictionary/")
 '(j-path "/opt/j64-701/" t)
 '(j-vocabulary-help-url "/opt/j64-701/help/dictionary/vocabul.htm")
 '(nodejs-repl-command "/home/troy/local/bin/node")
 '(nrepl-lein-command "/opt/scripts/lein")
 '(py-shell-toggle-1 "/usr/bin/python")
 '(py-shell-toggle-2 "/usr/bin/python3")
 '(py-split-windows-on-execute-function (quote split-window-horizontally))
 '(python-jython-command "/usr/bin/jython")
 '(python-python-command "/usr/bin/python")
 '(quack-default-program "/opt/racket/bin/racket")
 '(quack-programs (quote ("/opt/racket/bin/racket" "/opt/racket/bin/mzscheme" "bigloo" "csi" "csi -hygienic" "gosh" "gracket" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "mzscheme" "racket" "racket -il typed/racket" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi")))
 '(scala-interpreter "/opt/scala/scala/bin/scala")
 '(sml-program-name "/usr/bin/sml")
 '(tool-bar-mode nil)
 '(transient-mark-mode (quote (only . t))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(iedit-occurrence ((t (:inherit highlight :background "DeepPink3")))))


;; ============================================================================


;; ****************************
;; *                          *
;; * USER-SET INITIALIZATIONS *
;; *                          *
;; ****************************

(global-font-lock-mode t)
(setq use-file-dialog t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq transient-mark-mode t)
(setq column-number-mode t)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(delete-selection-mode 1)
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

(setq scroll-bar-mode-explicit t)
(set-scroll-bar-mode `right) 
(setq require-final-newline nil)
;(setq shift-select-mode nil)

(server-start)
(global-linum-mode t)
(icomplete-mode 99)
;; (iswitchb-mode 1)
(setq-default c-basic-offset 4)

(setq hscroll-margin 5)
(setq hscroll-step 1)

;(electric-pair-mode 1)

(setq x-select-enable-clipboard t)

(setq backup-by-copying-when-linked t)  ;; don't replace hard links

(add-to-list 'Info-default-directory-list "~/.emacs.d/info")

;; **********************
;; *                    *
;; * INITIALIZE PLUGINS *
;; *                    *
;; **********************

(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/plugins")

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

;; ****************
;; *              *
;; *  APPEARANCE  *
;; *              *
;; ****************

(setq my-themes '(adwaita zenburn misterioso sanity-inc-tomorrow-night
			  sanity-inc-tomorrow-eighties sanity-inc-tomorrow-day))
(setq light-theme 'adwaita)
(setq dark-theme 'sanity-inc-tomorrow-night)
(load-theme dark-theme t)
;; (setq current-theme dark-theme)

(defun theme (thm)
  (interactive "Stheme: ")
  (load-theme thm t)
  (setq current-theme thm))
  
(defun toggle-theme ()
  (interactive)
  (if (eq current-theme light-theme)
      (theme dark-theme)
    (theme light-theme)))

(set-face-attribute 'fringe nil :background "#555")  ;; set fringe to dark gray
(set-fringe-mode '(1 . 1))  ;; set fringe to 1px at left and right

  
;; variable-pitch-mode. (C-x C-9)
;; --------------------
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

;; ------
;; imenu.
;; ------
(defun try-to-add-imenu ()
  (condition-case nil (imenu-add-to-menubar "imenu") (error nil)))
(add-hook 'font-lock-mode-hook 'try-to-add-imenu)

;; ---------------
;; smart-mode-line
;; ---------------
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
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)


;; ***********
;; *         *
;; *  INPUT  *
;; *         *
;; ***********
(set-input-method 'TeX)
(defun load-math-input ()
  (interactive)
  (load "~/.emacs.d/math-input.el"))


;; **************************
;; *                        *
;; * SEARCHING & NAVIGATION *
;; *                        *
;; **************************

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
(require 'smex)
(autoload 'smex "smex" "Smex provides an ido interface to M-x commands.")
(global-set-key (kbd "M-x") 'smex)
(setq read-file-name-completion-ignore-case t)
(load "~/.emacs.d/ido-helm.el")
(define-key ido-buffer-completion-map (kbd "<tab>") 'ido-next-match)
(define-key ido-buffer-completion-map (kbd "<backtab>") 'ido-prev-match)


;; ;; anything
;; (require 'anything)

;; ---------
;; yasnippet
;; ---------
;; before auto-complete
(require 'yasnippet)
(yas-global-mode 1)
(setq yas-triggers-in-field t)  ; allow stacked expansion

;; ------------
;; auto-complete
;; ------------
(require 'auto-complete)
(add-hook 'prog-mode-hook (lambda () (auto-complete-mode t)))

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

;; -----
;; eldoc
;; -----
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; ------
;; ggtags
;; ------
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
(setq-default dired-listing-switches "-alhv")

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

(require 'uniquify) ;; remove in emacs24.4
;; (load "~/.emacs.d/init-popwin.el")

(windmove-default-keybindings 'meta)  ; meta-arrow to move buffers
(global-set-key [C-tab] 'next-multiframe-window)
(global-set-key [C-S-iso-lefttab] 'previous-multiframe-window)

;; -----------
;; buffer-move
;; -----------
(require 'buffer-move)
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

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
(global-set-key (kbd "<C-S-return>")  'win-swap)

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
;; (global-set-key (kbd "<C-return>")  #'cua-set-rectangle-mark)

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

;; ------------------
;; discover-my-major.
;; ------------------
(require 'discover-my-major)
(define-key help-map "\C-m" 'discover-my-major)



;; ----------
;; undo-tree.
;; ----------
(require 'undo-tree)
(global-undo-tree-mode)

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

(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)

(setq-default evil-symbol-word-search t)
(defun toggle-evil-symbol-word-search ()
  (interactive)
  (setf evil-symbol-word-search (not evil-symbol-word-search)))
(defalias 'evsw 'toggle-evil-symbol-word-search)

(defalias 'evl 'evil-local-mode)

;; Modes starting in Emacs state:
(loop for mode in
      '(comint-mode  ;; not working for comint-mode?
	dired-mode)
      do (add-to-list 'evil-emacs-state-modes mode))


(add-to-list 'evil-overriding-maps '(yas-minor-mode-map . insert))
(define-key evil-normal-state-map (kbd "TAB") #'indent-for-tab-command)
(define-key evil-normal-state-map (kbd "C-S-o") #'evil-jump-forward)
(define-key evil-normal-state-map (kbd "C-e") #'end-of-line)
(define-key evil-normal-state-map (kbd "C-:") #'evil-repeat-find-char-reverse)
(define-key evil-motion-state-map (kbd "C-e") #'end-of-line)
(define-key evil-insert-state-map (kbd "C-e") #'end-of-line)
(define-key evil-insert-state-map (kbd "C-S-y") #'evil-copy-from-below)
(define-key evil-insert-state-map (kbd "C-v") #'insert-char)
(define-key evil-normal-state-map (kbd "C-y") nil)
(define-key evil-insert-state-map (kbd "C-l") #'delete-char)
(define-key evil-insert-state-map (kbd "C-S-l") #'backward-delete-char)
(define-key evil-insert-state-map (kbd "C-k") #'evil-insert-digraph)
(define-key evil-insert-state-map (kbd "C-S-k") #'kill-line)

;; key bindings for other extensions
;; (define-key evil-visual-state-map (kbd "C-S-c C-S-c")
;;   (lambda () (interactive) (evil-mode nil) (setq mark-active t) (setq deactivate-mark nil)
;;     (exchange-point-and-mark) (mc/edit-lines)))
(define-key evil-visual-state-map (kbd "C-S-c C-S-c") 'mc/edit-lines)
(define-key evil-emacs-state-map (kbd "C-S-c C-S-c") 'mc/edit-lines)

(load "troy-evil-utils.el")
(define-key evil-visual-state-map [32] #'evil-forward-char-or-extend)

;; ESC quits everything:
(define-key evil-normal-state-map           [escape] 'keyboard-quit)
(define-key evil-visual-state-map           [escape] 'keyboard-quit)
(define-key minibuffer-local-map            [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map         [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map    [escape] 'minibuffer-keyboard-quit)

(evil-add-command-properties 'move-beginning-of-line-or-text :repeat 'ignore)


(setq evil-digraphs-table
      (delq (assoc '(?. ?.) evil-digraphs-table)
	    evil-digraphs-table))
(setq evil-digraphs-table-user 
      '(
	((?. ?.) . ?\x2026)
	((?, ?:) . ?\x2025)
	))
(defun evil-enter-digraphs nil
  (interactive)
  (with-demoted-errors
    (evil-insert-digraph 1)
    (evil-enter-digraphs)))
(defalias 'digra 'evil-enter-digraphs)

;; plugins...
(add-to-list 'load-path "~/.emacs.d/evil-plugins")


;; evil-exchange (vim-exchange)
(require 'evil-exchange)
;; (setq evil-exchange-key (kbd "zx"))
(evil-exchange-install)

;; evil-jumper
(require 'evil-jumper)

;; evil-nerd-commenter
(setq evilnc-hotkey-comment-operator ",,")
(require 'evil-nerd-commenter)
(evilnc-default-hotkeys)

;; evil-numbers
(require 'evil-numbers)
(global-set-key (kbd "C-c +") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-c =") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt)

;; evil-leader
(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(defun open-init-file ()
  (interactive)
  (find-file "~/.emacs"))
(evil-leader/set-key
  "e" 'find-file
  "b" 'switch-to-buffer
  "k" 'kill-buffer
  "." 'open-init-file
  "v" 'eval-buffer
  "a" 'ace-jump-word-mode
  "f" 'ace-jump-char-mode
  "g" 'ace-jump-line-mode
  )

;; evil-matchit - installed via package manager
(require 'evil-matchit)
(global-evil-matchit-mode 1)

;; evil-snipe
(add-to-list 'load-path "/home/troy/evil-snipe") ;; remove when merged 
(require 'evil-snipe)
(global-evil-snipe-mode 1)
;; ;; Optional!
;; (evil-snipe-replace-evil) ;; replaces evil-mode's f/F/t/T/;/, with snipe
;; (evil-snipe-enable-nN)    ;; enable repeating with n/N (not implemented)
;; ;; not necessary if using (evil-snipe-replace-evil)
;; (evil-snipe-enable-sS)    ;; enable repeating with s/S

;; evil-surround
(require 'surround)
(global-surround-mode 1)

;; evil-tabs
(require 'evil-tabs)
(global-evil-tabs-mode t)
(defun tabs ()
  (interactive)
  (elscreen-toggle-display-tab))

;; evil-visualstar
(require 'evil-visualstar)


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
(load "c++-utils.el")

;; make _ part of words
(modify-syntax-entry ?_ "w" c-mode-syntax-table)


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
(global-set-key [f9] 'evil-mode)



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
       '(("\\.pl$" . prolog-mode)
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

;; Insert Continuation Symbol.
(defun insert-continuation ()
  "insert \ at column 80"
  (interactive)
  (end-of-line)
  (insert-char 32 (- 79 (current-column)))
  (insert-char '?\\' 1))
(global-set-key "\C-x\\" 'insert-continuation)


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

;;---------------------------------------------------------------------------   

;; ZOOM IN AND OUT.

;; note: current buffer only, use text-scale-adjust(C-x C-+, C-x C--)

;; globally, zoom text and window...
;;by Vivek Haldar.
(defun zoom-in ()
  "Increase font size by 10 points"
  (interactive)
  (set-face-attribute 'default nil
		      :height
		      (+ (face-attribute 'default :height)
			 10)))
(defun zoom-out ()
  "Decrease font size by 10 points"
  (interactive)
  (set-face-attribute 'default nil
      		      :height
		      (- (face-attribute 'default :height)
		         10)))
;; change font size, interactively
(global-set-key (kbd "C-=") 'zoom-in)
(global-set-key (kbd "C-+") 'zoom-out)


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
  (find-file "~/.emacs.d/init.el"))


;; ------------------------------------------------------------------------
;; LOAD AUXILLARY FILES.

(setq aux-elisp-files
      '("~/.emacs.d/troy-utils.el"
	"~/.emacs.d/smooth-scrolling.el"
	"~/.emacs.d/move-text.el"))
(loop for f in aux-elisp-files do
      (if (file-exists-p f) (load f)))

;; *********
;; *       *
;; * MOUSE *
;; *       *
;; *********

;; free up S-down-mouse-1 and S-drag-mouse-1 for other bindings
(global-set-key (kbd "<C-S-down-mouse-1>") 'mouse-appearance-menu)
(global-set-key (kbd "<C-S-drag-mouse-1>") 'mouse-set-point)

(require 'mouse-copy)
(require 'mouse-drag)
;; These definitions override the old binding of M-mouse-1 to mouse-drag-secondary.
;; It can be made up with a M-mouse-1 followed by a M-mouse-3.
(global-set-key [M-down-mouse-1] 'mouse-drag-secondary-pasting)
(global-set-key [M-S-down-mouse-1] 'mouse-drag-secondary-moving)
(global-set-key [down-mouse-2] 'mouse-drag-drag)
(global-set-key [S-down-mouse-2] 'mouse-drag-throw)


;; ****************
;; *              *
;; * KEY BINDINGS *
;; *              *
;; ****************

(global-set-key (kbd "M-<backspace>") 'backward-kill-word)
(global-set-key (kbd "M-DEL") '(lambda () (interactive) (kill-buffer nil)))
(global-set-key (kbd "C-SPC") 'cua-set-mark)

(global-set-key [\M-\S-up] 'move-text-up)
(global-set-key [\M-\S-down] 'move-text-down)
(global-set-key [\M-\S-\delete] 'remove-current-line)
(global-set-key [\M-\S-\left] 'cut-current-line)
(global-set-key [\M-\S-\right] 'copy-current-line)


(global-set-key "\C-a" 'move-beginning-of-line-or-text)
;(global-set-key (kbd "M-;") 'comment-line-or-region)
(global-set-key (kbd "C-S-O") 'insert-line-above)

(global-set-key [f5] 'copy-region-as-kill) ; Copy
(global-set-key [f6] 'kill-region)         ; Cut
(global-set-key [f7] 'yank)                ; Paste
(global-set-key [kp-home]  'beginning-of-buffer) ; [Home]
(global-set-key [home]     'beginning-of-buffer) ; [Home]
(global-set-key [kp-end]   'end-of-buffer)       ; [End]
(global-set-key [end]      'end-of-buffer)       ; [End] 
(global-set-key [\M-f12]   'shell-pop)

(global-set-key (kbd "C-S-l") #'backward-delete-char)
;(global-set-key (kbd "M-;") 'comment-dwim)
;(global-set-key "\M-f" 'forward-sexp)
;(global-set-key "\M-b" 'backward-sexp)
(global-set-key "\M-sq" 'ucs-insert)
(global-set-key "\C-\M-f" 'forward-word)
(global-set-key "\C-\M-b" 'backward-word)
(global-set-key "\C-z" 'undo)    ; [Undo] 
;; (global-set-key "\M-1" 'goto-line)  ;; use "M-g g" or "M-g M-g"
(global-set-key "\M-c" 'capitalize-word)
(global-set-key (kbd "C-x C-9") 'variable-pitch-mode)
;(global-set-key "\C-h" 'delete-backward-char)
;(global-set-key "\C-?" 'help-command)
(global-set-key "\M-o" 'occur)
(global-set-key (kbd "C-x SPC") 'just-one-space)
(global-set-key [M-f1] 'apropos-follow)

;; Scrolling Up & Down.
(global-set-key (kbd "M-n") 'evil-scroll-line-down)
(global-set-key (kbd "M-p") 'evil-scroll-line-up)
(global-set-key (kbd "C-S-n")
		(lambda () (interactive) (scroll-other-window-down 1)))
(global-set-key (kbd "C-S-p")
		(lambda () (interactive) (scroll-other-window-down -1)))


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


;; ALIASES.
;; --------
(defalias 'reyas 'yas/reload-all)
(defalias 'boxcom 'box-heading-comment)
(defalias 'init 'load-init-file)
(defalias 'arv 'auto-revert-mode)
(defalias 'revb 'revert-buffer)
(defalias 'sim 'set-input-method)  ;; bound to C-\

;; FINAL
(setq skeleton-pair nil)
