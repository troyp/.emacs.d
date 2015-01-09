;;; evil-tabs.el --- Integrating Vim-style tabs for Evil mode users.
;; Copyright 2013 Kris Jenkins
;;
;; Author: Kris Jenkins <krisajenkins@gmail.com>
;; Maintainer: Kris Jenkins <krisajenkins@gmail.com>
;; Keywords: evil tab tabs vim
;; URL: https://github.com/krisajenkins/evil-tabs
;; Created: 30th September 2013
;; Version: 0.1.0
;; Package-Requires: ((evil "0.0.0") (elscreen "0.0.0"))

;;; Commentary:
;;
;; Integrating Vim-style tabs for Evil mode users.

;;; Code:

(require 'evil)
(require 'elscreen)

(defvar evil-tabs-mode-map (make-sparse-keymap)
  "Evil-tabs-mode's keymap.")

(evil-define-command evil-tabs-tabedit (file)
  (interactive "<f>")
  (elscreen-create)
  (find-file file))

(evil-ex-define-cmd "tabe[dit]" 'evil-tabs-tabedit)
(evil-ex-define-cmd "tabc[lose]" 'elscreen-kill)
(evil-ex-define-cmd "tabn[ew]" 'elscreen-create)

(evil-define-key 'normal evil-tabs-mode-map
  "gt" 'elscreen-next
  "gT" 'elscreen-previous)

;;;###autoload
(define-minor-mode evil-tabs-mode
  "Integrating Vim-style tabs for Evil mode users."
  :global t
  :keymap evil-tabs-mode-map
  (let ((prev-state evil-state))
    (evil-normal-state)
    (evil-change-state prev-state)

    (elscreen-start)))

;;;###autoload
(defun turn-on-evil-tabs-mode ()
  "Enable `evil-tabs-mode' in the current buffer."
  (evil-tabs-mode 1))

;;;###autoload
(defun turn-off-evil-tabs-mode ()
  "Disable `evil-tabs-mode' in the current buffer."
  (evil-tabs-mode -1))

;;;###autoload
(define-globalized-minor-mode global-evil-tabs-mode evil-tabs-mode turn-on-evil-tabs-mode)

(provide 'evil-tabs)

;;; evil-tabs.el ends here
