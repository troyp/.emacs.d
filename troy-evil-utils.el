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



