;; https://gist.github.com/abo-abo/8936648

(require 'helm-buffers)
(require 'delsel)

(add-hook
 'ido-setup-hook
 (lambda()
   (define-key ido-buffer-completion-map "\C-i" 'ido-buffer-helm)))

(defun ido-buffer-helm ()
  (interactive)
  (setq ibh-initial ido-text)
  (add-hook 'post-command-hook 'ibh-hook)
  (minibuffer-keyboard-quit))

(defvar ibh-initial nil)

(defun ibh-hook ()
  (when ibh-initial
    (let ((str ibh-initial))
      (setq ibh-initial)
      (helm :sources '(helm-source-buffers-list
                       helm-source-ido-virtual-buffers
                       helm-source-buffer-not-found)
            :buffer "*helm buffers*"
            :keymap helm-buffer-map
            :truncate-lines t
            :input str)))
  (remove-hook 'post-command-hook 'ibh-hook))

(defvar is-initial nil)

(defun ido-smex ()
  (interactive)
  (setq is-initial ido-text)
  (unless (string= is-initial "")
    (setq is-initial (concat ido-text " ")))
  (add-hook 'post-command-hook
            'is-hook)
  (minibuffer-keyboard-quit))

(defun is-hook ()
  (remove-hook 'post-command-hook 'is-hook)
  (helm-M-x-mine is-initial))

(eval-after-load 'smex
  `(defun smex-prepare-ido-bindings ()
     (define-key ido-completion-map (kbd "TAB") 'minibuffer-complete)
     (define-key ido-completion-map (kbd "C-,") 'smex-describe-function)
     (define-key ido-completion-map (kbd "C-w") 'smex-where-is)
     (define-key ido-completion-map (kbd "C-.") 'smex-find-function)
     (define-key ido-completion-map (kbd "C-a") 'move-beginning-of-line)
     (define-key ido-completion-map "\C-i" 'ido-smex)))

(require 'helm-command)

(defun helm-M-x-mine (&optional initial-input)
  "Preconfigured `helm' for Emacs commands.
It is `helm' replacement of regular `M-x' `execute-extended-command'.
Optional INITIAL-INPUT will be used to complete command name."
  (interactive)
  (let* ((history (cl-loop for i in extended-command-history
                           when (commandp (intern i)) collect i))
         command sym-com in-help help-cand
         (helm--mode-line-display-prefarg t)
         (pers-help #'(lambda (candidate)
                        (let ((hbuf (get-buffer (help-buffer))))
                          (if (and in-help (string= candidate help-cand)
                                   (null helm-persistent-action-use-special-display))
                              (progn
                                ;; When M-x is started from a help buffer,
                                ;; Don't kill it as it is helm-current-buffer.
                                (unless (equal hbuf helm-current-buffer)
                                  (kill-buffer hbuf)
                                  (set-window-buffer (get-buffer-window hbuf)
                                                     helm-current-buffer))
                                (setq in-help nil))
                              (helm-describe-function candidate)
                              (setq in-help t))
                          (setq help-cand candidate))))
         (tm (run-at-time 1 0.1 'helm-M-x--notify-prefix-arg)))
    (setq current-prefix-arg nil)
    (unwind-protect
         (setq command (helm-comp-read
                        "M-x " obarray
                        :test 'commandp
                        :requires-pattern helm-M-x-requires-pattern
                        :name "Emacs Commands"
                        :buffer "*helm M-x*"
                        :persistent-action pers-help
                        :persistent-help "Describe this command"
                        :history history
                        :reverse-history helm-M-x-reverse-history
                        :del-input nil
                        :mode-line helm-mode-line-string
                        :must-match t
                        :keymap helm-map
                        :candidates-in-buffer t
                        :fc-transformer 'helm-M-x-transformer
                        :initial-input initial-input))
      (cancel-timer tm)
      (setq helm--mode-line-display-prefarg nil))
    (setq sym-com (intern command))
    (setq current-prefix-arg helm-current-prefix-arg)
    ;; Avoid having `this-command' set to *exit-minibuffer.
    (setq this-command sym-com
          ;; Handle C-x z (repeat) Issue #322
          real-this-command sym-com)
    ;; This ugly construct is to save history even on error.
    (unless helm-M-x-always-save-history
      (call-interactively sym-com))
    (setq extended-command-history
          (cons command (delete command history)))
    (when helm-M-x-always-save-history
      (call-interactively sym-com))))

(provide 'ido-helm)
