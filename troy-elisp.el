
(defun ep (expr)
  (newline)
  (princ ">> " (point-marker))
  (princ expr (point-marker)))
