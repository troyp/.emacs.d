;; from http://www.emacswiki.org/emacs/TeXInputMethod

;; ********************************
;; *                              *
;; * Customizing Tex Input Method *
;; *                              *
;; ********************************
(let ((quail-current-package (assoc "TeX" quail-package-alist)))
  (quail-define-rules ((append . t))
		      ("^\\alpha" ?ᵅ)))

;; ************************************
;; *                                  *
;; * An Alternative Math Input Method *
;; *                                  *
;; ************************************
(package-initialize)
(require 'math-symbol-lists)
(quail-define-package "math" "UTF-8" "Ω" t)
(quail-define-rules ; add whatever extra rules you want to define here...
 ("\\from"    #X2190)
 ("\\to"      #X2192)
 ("\\lhd"     #X22B2)
 ("\\rhd"     #X22B3)
 ("\\unlhd"   #X22B4)
 ("\\unrhd"   #X22B5))
(mapc (lambda (x)
        (if (cddr x)
            (quail-defrule (cadr x) (car (cddr x)))))
      (append math-symbol-list-basic math-symbol-list-extended))

;; To activate the input method, type `C-u C-\ math RET’.
;; Then, typing \mscrC yields Script C, \lParen yields ⦅, \gamma\dot yields γ̇, etc
