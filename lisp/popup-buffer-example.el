;; http://stackoverflow.com/questions/19462168/emacs-toggle-a-specific-window
;; code by artscan

(require 'popwin)
(popwin-mode 1)

(generate-new-buffer "special-buffer")

(setq eab/special-buffer-displaedp nil)
(setq eab/special-buffer "special-buffer")

(add-to-list 'popwin:special-display-config
         `(,eab/special-buffer :width 20 :position left :stick t))    

(defun eab/special-buffer-toggle ()
  (interactive)
  (if eab/special-buffer-displaedp
      (progn
      (popwin:close-popup-window)
      (setq eab/special-buffer-displaedp nil))
    (progn
      (ignore-errors (popwin:display-buffer eab/special-buffer))
      (setq eab/special-buffer-displaedp 't))))

(global-set-key (kbd "<f3>") 'eab/special-buffer-toggle)
