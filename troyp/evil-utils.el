;; ********************
;; *                  *
;; * HELPER FUNCTIONS *
;; *                  *
;; ********************
(defun printf (s)
  (print s (lambda (c) nil)))

(defun print-list-elements (list &optional PRINTCHARFUN)
  (while list
    (print (car list) PRINTCHARFUN)
    (setq list (cdr list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun evil-forward-char-or-extend ()
  ;; FIXME
   (interactive)
   (if (eq (point) (- (line-end-position) 1))
       (progn
	(replace-string "\n" " \n" nil (point) (+ (line-end-position) 1))
	;; (backward-char))
	(evil-visual-restore))
     (evil-forward-char)))


;; *************
;; *           *
;; * JUMP LIST *
;; *           *
;; *************

(if (boundp 'popwin:special-display-config)
    (push "*jumps*" popwin:special-display-config))
(defun jumps ()
  (interactive)
  (with-output-to-temp-buffer "*jumps*"
    (print-list-elements evil-jump-list)))


;; ************
;; *          *
;; * DIGRAPHS *
;; *          *
;; ************

(defun evil-enter-digraphs nil
  (interactive)
  (with-demoted-errors
    (evil-insert-digraph 1)
    (evil-enter-digraphs)))


;; ***********************************
;; *                                 *
;; * LOWER-CASE PARENS IN LISP MODES *
;; *                                 *
;; ***********************************

;; racket-mode, emacs-lisp-mode, lisp-mode
(defun insert-lparen ()
  (interactive)
  (insert ?\())
(defun insert-rparen ()
  (interactive)
  (insert ?\)))
(defun insert-lbrack ()
  (interactive)
  (insert ?\[))
(defun insert-rbrack ()
  (interactive)
  (insert ?\]))
(defun lowercase-parens-in-mode-map (mode-map)
  (evil-define-key 'insert mode-map (kbd "[") 'insert-lparen)
  (evil-define-key 'insert mode-map (kbd "]") 'insert-rparen)
  (evil-define-key 'insert mode-map (kbd "(") 'insert-lbrack)
  (evil-define-key 'insert mode-map (kbd ")") 'insert-rbrack))
(defun uppercase-parens-in-mode-map (mode-map)
  (evil-define-key 'insert mode-map (kbd "[") 'self-insert-command)
  (evil-define-key 'insert mode-map (kbd "]") 'self-insert-command)
  (evil-define-key 'insert mode-map (kbd "(") 'self-insert-command)
  (evil-define-key 'insert mode-map (kbd ")") 'self-insert-command))
(defvar lisp-mode-maps
  '( emacs-lisp-mode-map
     lisp-mode-map
     racket-mode-map ))
(defun lowercase-parens-in-lisp-modes ()
  (interactive)
  (if (not (boundp 'lowercase-parens-p)) (defvar lowercase-parens-p nil))
  (if (not lowercase-parens-p)
      (progn
	(setf lowercase-parens-p t)
	(dolist (mode-map lisp-mode-maps)
	  (lowercase-parens-in-mode-map mode-map)))
    (progn
	(setf lowercase-parens-p nil)
	(dolist (mode-map lisp-mode-maps)
	  (uppercase-parens-in-mode-map mode-map)))))

