;; chomp: from Emacs Lisp Cookbook
(defun chomp (str)
      "Chomp leading and tailing whitespace from STR."
      (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                           str)
        (setq str (replace-match "" t t str)))
      str)


(defun insert-enum-class-operator<< (enum-name contents)
  (interactive)
  (insert "ostream& operator<<(ostream& out, " enum-name " x) {")
  (c-indent-line-or-region)
  (insert "\nswitch(x) {")
  (c-indent-line-or-region)
  (let* ((names (mapcar #'chomp (split-string contents ",")))
	 (name-field-length (apply #'max (mapcar #'length names))))
    (cl-loop for name in names
	     do (progn
		  (let ((filler (make-string (- name-field-length (length name))
					     32)))
		    (insert "\n\tcase " enum-name "::" name filler ":\t" 
			    "return out << \"" name "\";")
		    (c-indent-line-or-region))))
    (insert "\n}") (c-indent-line-or-region)
    (insert "\n}") (c-indent-line-or-region)
    (insert "\n")))

(defun align-c-end-comment (BEG END)
  (interactive "r")
  (align-regexp BEG END "\\(\\s-*\\)\\*/"))
