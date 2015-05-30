;; ===========================================================================
;;        ___________________ 
;;       |                   |
;;       | HEADING FUNCTIONS |
;;       |___________________|

(load "troyp/utils.el")

;; ---------------------------------------------------------------------------
;; ,-----------------------,
;; | Box Heading Functions |
;; '-----------------------'

(defvar *box-heading-symbol* ?*)
(defvar *box-heading-margin* 1)
(defvar *box-heading-vmargin* 1)
(defvar *banner-heading-symbol* ?=)
(defvar *heading-symbol* ?-)

(defun box-heading-lines (s)
  (interactive)
  (unless (has-value-p *box-heading-symbol*) (setq *box-heading-symbol* "**"))
  (unless (has-value-p *box-heading-margin*) (setq *box-heading-margin* 1))
  (unless (has-value-p *box-heading-vmargin*) (setq *box-heading-vmargin* *box-heading-margin*))
  (let* ((lines (split-string s "\n"))
	 (n     (apply #'max (mapcar 'length lines)))
	 (d     (or *box-heading-margin* 1))
	 (syms  (if (characterp *box-heading-symbol*)
		    (string *box-heading-symbol* *box-heading-symbol*)
		  *box-heading-symbol*))
	 (nsyms (length syms))
	 (hsym         (elt syms 0))
	 (hsym-bottom  (if (< nsyms 8) hsym
			 (elt syms 1)))
	 (vsym         (if (< nsyms 8) (elt syms 1)
			 (elt syms 2)))
	 (vsym-right   (if (< nsyms 8) vsym
			 (elt syms 3)))
	 (cnrs         (cond ((= nsyms 2) (make-string 4 hsym))
			     ((= nsyms 3) (make-string 4 (elt syms 2)))
			     ((= nsyms 6) (subseq syms 2))
			     ((= nsyms 8) (subseq syms 4))))
	 (nw (string (aref cnrs 0)))
	 (ne (string (aref cnrs 1)))
	 (sw (string (aref cnrs 2)))
	 (se (string (aref cnrs 3)))
	 (spc   32)    ; space
	 (N     (+ n 2 (* 2 d)))
	 (v-margin         (make-string d spc))
	 (h-border-top     (concat nw (make-string (- N 2) hsym) ne))
	 (h-border-bottom  (concat sw (make-string (- N 2) hsym-bottom) se))
	 (h-margin-line    (concat (string vsym) (make-string (- N 2) spc) (string vsym-right)))
	 (h-margin         (make-list *box-heading-vmargin* h-margin-line))
	 (textlines        (mapcar (lambda (s)
				     (concat (string vsym) v-margin s
					     (make-string (- n (length s)) spc)
					     v-margin (string vsym-right)))
				   lines)))
    (flatlines h-border-top
	       h-margin
	       textlines
	       h-margin
	       h-border-bottom)))

(defun box-heading-custom-lines (s syms)
  (let ((*box-heading-symbol* syms))
    (box-heading-lines s)))

;; ---------------------------------------------------------------------------
;; ,-------------------------------,
;; | Rectangular Heading Functions |
;; '-------------------------------'

;; Borders can be omitted to join adjacent rectangles

(defvar *rect-heading-omit-left*)
(defvar *rect-heading-omit-right*)
(defvar *rect-heading-omit-top*)
(defun rect-heading-lines (s)
  (interactive)
  (unless (has-value-p *rect-heading-omit-top*)   (setq *rect-heading-omit-top* nil))
  (unless (has-value-p *rect-heading-omit-left*)  (setq *rect-heading-omit-left* nil))
  (unless (has-value-p *rect-heading-omit-right*) (setq *rect-heading-omit-right* nil))
  (let* ((lines (split-string s "\n"))
	 (n     (apply #'max (mapcar 'length lines)))
	 (d     (or *box-heading-margin* 1))
	 (spc   32)    ; space
	 (N     (+ n 2 (* 2 d)))
	 (lspc  (if *rect-heading-omit-left* "" " "))
	 (rspc  (if *rect-heading-omit-right* "" " "))
	 (l  (if *rect-heading-omit-left* "" "|"))
	 (r  (if *rect-heading-omit-right* "" "|"))

	 (v-margin         (make-string d spc))
	 (h-top            (concat lspc (make-string (- N 2) ?_) rspc))
	 (h-bottom         (concat l (make-string (- N 2) ?_) r))
	 (h-margin         (concat l (make-string (- N 2) spc) r))
	 (textlines        (mapcar
			    (lambda (s)
			      (concat l v-margin s
				      (make-string (- n (length s)) spc)
				      v-margin r))
			    lines))
	 (lines            (append
			    (if *rect-heading-omit-top* nil (list h-top))
			    (list h-margin textlines h-bottom))))
    (funcall 'flatlines lines)))

(defun short-rect-heading-lines (s)
  (let ((*box-heading-vmargin* 0)
	(*box-heading-symbol* "--||,,''"))
    (box-heading-lines s)))


;; ===========================================================================
;;        __________________ 
;;       |                  |
;;       | HEADING COMMANDS |
;;       |__________________|


;; ---------------------------------------------------------------------------
;; ,-----------------------------,
;; | Plain Text Heading Commands |
;; '-----------------------------'

(defun make-heading-command (heading-lines-fn-sym)
  `(lambda (s)
     (interactive "s")
     (insert-lines (,heading-lines-fn-sym s))))

(fset 'box-heading (make-heading-command 'box-heading-lines))
(fset 'rect-heading (make-heading-command 'rect-heading-lines))
(fset 'short-rect-heading (make-heading-command 'short-rect-heading-lines))

(defun rect-heading-under (s)
  (let ((*rect-heading-omit-top* 't))    (rect-heading s)))
(defun rect-heading-right (s)
  (let ((*rect-heading-omit-left* 't))   (rect-heading s)))
(defun rect-heading-left (s)
  (let ((*rect-heading-omit-right* 't))  (rect-heading s)))
(defun rect-heading-above (s)
  (let ((*rect-heading-omit-bottom* 't)) (rect-heading s)))

;; ---------------------------------------------------------------------------
;; ,--------------------------,
;; | Heading Comment Commands |
;; '--------------------------'

(defun make-heading-comment (heading-fn-sym)
  `(lambda (s)
     (interactive "s" "heading: ")
     (let ((start (point)))
	 (,heading-fn-sym s)
	 (comment-region start (point)))))

(fset 'box-heading-comment  (make-heading-comment 'box-heading))
(fset 'rect-heading-comment (make-heading-comment 'rect-heading))
(fset 'short-rect-heading-comment (make-heading-comment 'short-rect-heading))


;; ===========================================================================
;;        ____________________________ 
;;       |                            |
;;       | DIVIDER & SECTION COMMANDS |
;;       |____________________________|


;; ---------------------------------------------------------------------------
;; ,------------------,
;; | Divider Commands |
;; '------------------'

(defvar *divider-char* ?-)
(defun divider (n)
  (interactive "p")
  (if (= n 1) (setf n 75))
  (move-beginning-of-line nil)
  (insert (make-string n *divider-char*))
  (newline))

(defun divider-thick (n)
  (interactive "p")
  (let ((*divider-char* ?=))
    (if (= n 1) (setf n 75))
    (move-beginning-of-line nil)
    (insert (make-string n *divider-char*))
    (newline)))

(defun divider-comment (n)
  (interactive "p")
  (with-comment divider n))
(defun divider-thick-comment (n)
  (interactive "p")
  (with-comment divider-thick n))

;; ---------------------------------------------------------------------------
;; ,------------------,
;; | Section Commands |
;; '------------------'

(defun srh-section (s &optional n)
  (interactive "sheading: \np")
  (if (= n 1) (setf n 75))
  (push-mark (point))
  (divider n)
  (insert-lines (short-rect-heading-lines s))
  (open-line 1)
  (comment-region (mark) (point))
  (indent-region  (mark) (point))
  (next-line)
  (pop-mark))

(defun rh-section (s &optional n &optional heading-indent)
  (interactive "sheading: \np")
  (unless heading-indent (setf heading-indent 6))
  (if (= n 1) (setf n 75))
  (push-mark (point))
  (let ((*divider-char* ?=))
    (divider n))
  (insert-lines (indent-lines (rect-heading-lines s)
			      heading-indent))
  (open-line 1)
  (comment-region (mark) (point))
  (next-line)
  (pop-mark))
