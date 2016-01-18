;; from http://emacs.stackexchange.com/questions/653/#answer-654

(defun key-binding-at-point (key) 
  (mapcar (lambda (keymap) (when (keymapp keymap) 
							 (lookup-key keymap key))) 
		  (list 
		   ;; More likely 
		   (get-text-property (point) 'keymap) 
		   (mapcar (lambda (overlay) (overlay-get overlay 'keymap)) 
				   (overlays-at (point)))
		   ;; Less likely 
		   (get-text-property (point) 'local-map) 
		   (mapcar (lambda (overlay) (overlay-get overlay 'local-map)) 
				   (overlays-at (point))))))

(defun locate-key-binding (key)
  "Determine in which keymap KEY is defined."
  (interactive "kPress key: ")
  (let ((ret
         (list
          (key-binding-at-point key)
          (minor-mode-key-binding key)
          (local-key-binding key)
          (global-key-binding key))))
    (when (called-interactively-p 'any)
      (message "At Point: %s\nMinor-mode: %s\nLocal: %s\nGlobal: %s"
               (or (nth 0 ret) "") 
               (or (mapconcat (lambda (x) (format "%s: %s" (car x) (cdr x)))
                              (nth 1 ret) "\n             ")
                   "")
               (or (nth 2 ret) "")
               (or (nth 3 ret) "")))
    ret))
