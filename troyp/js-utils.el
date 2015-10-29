
(defun expand-bookmarklet ()
  (interactive)
  (let ((a (region-beginning))
	(b (region-end)))
    (defun replacefn (FROM TO)
      (save-excursion
	(goto-char a)
	(while (search-forward FROM b t)
	  (replace-match TO nil t))))
    ;; (shell-command-on-region a b
    ;; 			     "sh -c xargs -0 | /usr/bin/urlencode -d"
    ;; 			     (current-buffer)
    ;; 			     t
    ;; 			     "*Errors: expand-bookmarklet*" t)
    (replacefn "%20" " ")
    (replacefn "{" "{\n")
    (replacefn "}" "\n}\n")
    (replacefn ";" ";\n")))

(defun expand-css ()
  (interactive)
  (let ((a (region-beginning))
	(b (region-end)))
    (defun replacefn (FROM TO)
      (save-excursion
	(goto-char a)
	(while (search-forward FROM b t)
	  (replace-match TO nil t))))
    (replacefn "{" "{\n")
    (replacefn "}" "\n}\n")
    (replacefn ";" ";\n")))
