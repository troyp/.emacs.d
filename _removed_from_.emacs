;; ************
;; *          *
;; *  GENERAL *
;; *          *
;; ************

;; ALGS4 COURSE.
;; expand specification to empty method bodies
(setq expand-sig "\C-s)\C-m {\C-j\C-e\C-j}\C-n")


;; **************
;; *            *
;; * EXTENSIONS *
;; *            *
;; **************

;; mark-forward-sexp (a la vim text objects)
;; note: work with delimited expressions, not sexprs
;; note: additional delimiter pairs can be defined as follows:
;; (modify-syntax-entry ?< "(>")
;; (modify-syntax-entry ?> ")<")
;; the first arg specifies a char, the second defines it as open/close
;; delimiter and gives its matching delimiter
;; an optional syntax gives the mode-syntax-table to modify
(require 'mark-forward-sexp)
(global-set-key (kbd "C-c o") 'mark-forward-sexp)
(global-set-key (kbd "C-c O") 'mark-backward-sexp)
(global-set-key (kbd "C-c i") 'mark-inside-forward-sexp)
(global-set-key (kbd "C-c I") 'mark-inside-backward-sexp)
(defun mark-surrounding-sexp (char &optional arg)
  (interactive "c\npinitial delimiter: ")
  (dotimes (_ (or arg 1))
    (skip-chars-backward (string ?^ ?\\ char))
    (backward-char 1))
  (mark-forward-sexp char)
  (setq deactivate-mark nil))
(defun mark-inside-surrounding-sexp (char &optional arg)
  (interactive "c\npinitial delimiter: ")
  (dotimes (_ (or arg 1))
    (skip-chars-backward (string ?^ ?\\ char))
    (backward-char 1))
  (mark-inside-forward-sexp char)
  (setq deactivate-mark nil))
(global-set-key (kbd "C-c u") 'mark-surrounding-sexp)
(global-set-key (kbd "C-c U") 'mark-inside-surrounding-sexp)

