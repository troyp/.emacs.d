
;; *********
;; *       *
;; * MOUSE *
;; *       *
;; *********

;; free up S-down-mouse-1 and S-drag-mouse-1 for other bindings
(global-set-key (kbd "<C-S-down-mouse-1>") 'mouse-appearance-menu)
(global-set-key (kbd "<C-S-drag-mouse-1>") 'mouse-set-point)

(require 'mouse-copy)
(require 'mouse-drag)
;; These definitions override the old binding of M-mouse-1 to mouse-drag-secondary.
;; It can be made up with a M-mouse-1 followed by a M-mouse-3.
(global-set-key [M-down-mouse-1] 'mouse-drag-secondary-pasting)
(global-set-key [M-S-down-mouse-1] 'mouse-drag-secondary-moving)
(global-set-key [down-mouse-2] 'mouse-drag-drag)
(global-set-key [S-down-mouse-2] 'mouse-drag-throw)
