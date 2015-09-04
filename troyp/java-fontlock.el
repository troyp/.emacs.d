(defface font-lock-type-param-face
  `((t (:inherit font-lock-type-face
	:foreground "#DF00E800EF00")))
  "Face for type parameters")
  
;; crude test version
(defun font-lock-java-extra ()
  (font-lock-add-keywords
   nil
   '(("<\\([A-Za-z_][A-Za-z0-9_]*\\)>" 1 font-lock-type-face)
     ;; ("\\([A-Za-z_][A-Za-z0-9_]*\\)\\(<\\)\\([?]\\|[A-Za-z_][A-Za-z0-9_]*\\)\\s-+\\(super\\|extends\\)\\s-+[^>]*\\(>\\)"
     ;;  1 font-lock-type-face
      ;; 2 font-lock-keyword-face
      ;; 3 font-lock-type-param-face
      ;; 4 font-lock-keyword-face
      ;; 5 font-lock-keyword-face)
     )))

;; ;; FIXME
;; (defun font-lock-java-extra ()
;;   (font-lock-add-keywords
;;    nil
;;    '(
;;      ("\\([A-Za-z_][A-Za-z0-9_]*\\)\\(<\\)\\([A-Za-z_][A-Za-z0-9_]*\\)\\(>\\)"
;;       1 font-lock-type-face
;;       2 font-lock-keyword-face
;;       3 font-lock-type-param-face
;;       4 font-lock-keyword-face)
;;      ("\\([A-Za-z_][A-Za-z0-9_]*\\)\\(<\\)\\([?]\\|[A-Za-z_][A-Za-z0-9_]*\\)\\s-+\\(super\\|extends\\)\\s-+[^>]*\\(>\\)"
;;       1 font-lock-type-face
;;       2 font-lock-keyword-face
;;       3 font-lock-type-param-face
;;       4 font-lock-keyword-face
;;       5 font-lock-keyword-face)
;;      )))

(add-hook 'java-mode-hook 'font-lock-java-extra)

(defun font-lock-fixme ()
  (font-lock-add-keywords 
   nil
   '(("\\<\\(FIXME\\):" 1 font-lock-warning-face prepend))))
     
(add-hook 'java-mode-hook 'font-lock-fixme) 


