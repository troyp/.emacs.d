(fset 'id 'identity)

(defmacro has-value-p (sym)
  "(has-value-p SYM): Returns t if SYM is bound and non-null, nil otherwise"
  `(and (boundp ',sym) ,sym t))

(defun zap-upto-char (arg char)
  (interactive "p\nczap up to char: ")
  (zap-to-char arg char)
  (insert char)
  (backward-char))

(defun lines (&rest ss)
  (mapconcat 'id ss "\n"))

(defun flatlines (&rest ls)
  (if (null ls)
      ""
    (let ((x  (car ls))
	  (xs (cdr ls)))
      (cond
       ((stringp x) (lines x (flatlines xs)))
       ((listp x)   (apply #'flatlines (append x xs)))))))

(defun repeat (n s)
  (apply 'concat (make-list n s)))

(defun explode (s)
  (string-split s ""))

(defun printed (s)
  "Returns the printed representation of object s."
  (print s (lambda (c) nil)))

(defun printb (s)
  "Output printed representation of object to current buffer
   Formats as per 'print, with newlines around the representation.
   Also returns the representation"
  (print s (current-buffer)))

(defun print-list-elements (list &optional PRINTCHARFUN)
  "print the elements of LIST, one to a line.
   Optional argument PRINTCHARFUN specifies the output stream.
   If omitted, the value of `standard-output' is used.
   Possible values:
     - a buffer (inserted at point)
     - a marker
     - a function (will be called in turn with each character of output)
     - a symbol (its function definition is used)
     - t (output displayed in echo area)"
  (while list
    (print (car list) PRINTCHARFUN)
    (setq list (cdr list))))

(defun ep (expr)
  (newline)
  (princ ">> " (point-marker))
  (princ expr (point-marker)))

(defun concat-symbols (&rest syms)
  (make-symbol (apply 'concat (mapcar 'symbol-name syms))))


;; *************************
;; *                       *
;; * BOX HEADING FUNCTIONS *
;; *                       *
;; *************************

(defvar *box-heading-symbol* ?*)
(defvar *box-heading-margin* 1)
(defvar *box-heading-vmargin* 1)
(defvar *banner-heading-symbol* ?=)
(defvar *heading-symbol* ?-)

(defun box-heading-string (s)
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

(defun box-heading-custom-string (s syms)
  (let ((*box-heading-symbol* syms))
    (box-heading-string s)))


;;  ______________________ 
;; |                      |
;; | RECTANGULAR HEADINGS |
;; |______________________|
;;
;; Borders can be omitted to join adjacent rectangles

(defvar *rect-heading-omit-left*)
(defvar *rect-heading-omit-right*)
(defvar *rect-heading-omit-top*)
(defun rect-heading-string (s)
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

(defun rect-heading-under (s)
  (let ((*rect-heading-omit-top* 't))
    (rect-heading s)))
(defun rect-heading-right (s)
  (let ((*rect-heading-omit-left* 't))
    (rect-heading s)))
(defun rect-heading-left (s)
  (let ((*rect-heading-omit-right* 't))
    (rect-heading s)))
(defun rect-heading-above (s)
  (let ((*rect-heading-omit-bottom* 't))
    (rect-heading s)))

(defun short-rect-heading-string (s)
  (let ((*box-heading-vmargin* 0)
	(*box-heading-symbol* "--||,,''"))
    (box-heading-string s)))


;;  __________________ 
;; |                  |
;; | HEADING COMMANDS |
;; |__________________|

(defun make-heading-command (heading-string-fn-sym)
  `(lambda (s)
     (interactive "s")
     (insert (,heading-string-fn-sym s))))

(fset 'box-heading (make-heading-command 'box-heading-string))
(fset 'rect-heading (make-heading-command 'rect-heading-string))
(fset 'short-rect-heading (make-heading-command 'short-rect-heading-string))

(defun make-heading-comment (heading-fn-sym)
  `(lambda (s)
     (interactive "s" "heading: ")
     (let ((start (point)))
	 (,heading-fn-sym s)
	 (comment-region start (point)))))

(fset 'box-heading-comment  (make-heading-comment 'box-heading))
(fset 'rect-heading-comment (make-heading-comment 'rect-heading))

;; 2-argument commands
;; TODO: write make-heading-command taking variable arguments
(defun make-heading-command-2 (heading-string-fn-sym param-name)
  `(lambda (s param)
     (interactive "s" param-name)
     (insert (,heading-string-fn-sym s param))))
(fset 'box-heading-custom (make-heading-command-2 'box-heading-custom-string "syms"))


(defun srh-section (s &optional n &optional ch)
  (interactive "sheading: ")
  (unless n (setf n 65))
  (unless ch (setf ch ?-))
  (let ((divider (make-string n ch)))
    (insert
     (flatlines
      (list divider
	    (short-rect-heading-string s)
	    divider)))))


;; *******************************
;; *                             *
;; * LINE-BASED EDITING COMMANDS *
;; *                             *
;; *******************************

(defun remove-current-line ()
  (interactive)
  (let ((orig-col (current-column)))
    (move-beginning-of-line nil)
    (kill-whole-line)
    (move-to-column orig-col)))
(defun cut-current-line ()
  (interactive)
  (let ((orig-col (current-column)))
    (move-beginning-of-line nil)
    (append-next-kill)
    (kill-whole-line)
    (move-to-column orig-col)))
(defun copy-current-line ()
  (interactive)
  (let ((orig-col (current-column)))
    (move-beginning-of-line nil)
    (push-mark (point))
    (move-end-of-line nil)
    (kill-ring-save (mark) (point))
    (newline)
    (yank)
    (move-to-column orig-col)))

;; open-line functions:
;; FIXME: functions work, but when reversed, leave cursor where new line was

(defun open-line-above (n)
  (interactive "p")
  (save-excursion
    (push (point) buffer-undo-list)
    (previous-line 1)
    (move-end-of-line nil)
    (newline n)))

(defun open-line-below (n)
  (interactive "p")
  (save-excursion
    (push (point) buffer-undo-list)
    (move-end-of-line nil)
    (open-line n)))


;; ***********************************
;; *                                 *
;; * ECLIPSE-STYLE BEGINNING-OF-LINE *
;; *                                 *
;; ***********************************

;; adapted from move-beginning-of-line from simple.el (emacs source)
;; modified by Troy Pracy
;; license: emacs license (GPL3 or later)
(defun move-beginning-of-line-or-text (arg)
  "Move point to beginning of current line as displayed.
\(If there's an image in the line, this disregards newlines
which are part of the text that the image rests on.)

With argument ARG not nil or 1, move forward ARG - 1 lines first.
If point reaches the beginning or end of buffer, it stops there.
To ignore intangibility, bind `inhibit-point-motion-hooks' to t."
  (interactive "^p")
  (or arg (setq arg 1))

  (let ((orig (point))
	first-vis first-vis-field-value)

    ;; Move by lines, if ARG is not 1 (the default).
    (if (/= arg 1)
	(let ((line-move-visual nil))
	  (line-move (1- arg) t)))

    ;; Move back to indentation
    (back-to-indentation)
    (when (= (point) orig)
      ;; Move to beginning-of-line, ignoring fields and invisible text.
      (skip-chars-backward "^\n")
      (while (and (not (bobp)) (invisible-p (1- (point))))
	(goto-char (previous-char-property-change (point)))
	(skip-chars-backward "^\n"))

      ;; Now find first visible char in the line
      (while (and (not (eobp)) (invisible-p (point)))
	(goto-char (next-char-property-change (point))))
      (setq first-vis (point))

      ;; See if fields would stop us from reaching FIRST-VIS.
      (setq first-vis-field-value
	    (constrain-to-field first-vis orig (/= arg 1) t nil))

      (goto-char (if (/= first-vis-field-value first-vis)
		     ;; If yes, obey them.
		     first-vis-field-value
		   ;; Otherwise, move to START with attention to fields.
		   ;; (It is possible that fields never matter in this case.)
		   (constrain-to-field (point) orig
				       (/= arg 1) t nil))))))


;; *******************************
;; *                             *
;; * FILE MANIPULATION FUNCTIONS *
;; *                             *
;; *******************************
(defun rename-file-and-buffer (name)
  (interactive "snew name: ")
  (let ((orig-name (buffer-name)))
    (if orig-name
  	(progn
  	  (set-visited-file-name name t)
  	  (save-buffer)
  	  (delete-file orig-name))
      (if (y-or-n-p "No file associated with buffer. Create new file? ")
  	  (write-file name)
  	(error "aborted")))
    (message name orig-name)))

(defun sudo-open-file (filename &optional wildcards)
  (interactive
   (find-file-read-args "Find file: "
                        (confirm-nonexistent-file-or-buffer)))
  (require 'tramp)
  (let ((f (concat "/sudo::" (expand-file-name filename))))
    (find-file f wildcards)))


;; ;; ************
;; ;; *          *
;; ;; * COMMENTS *
;; ;; *          *
;; ;; ************
; functionality replaced by evil-nerd-commenter

;; (defun comment-line (arg)
;;   "Comment out or uncomment a single line, n lines below the current line."
;;   (interactive "*P")
;;   (save-excursion
;;     (let ((n (cond
;; 	      ((not arg) 0)
;; 	      ((stringp arg) (string-to-number arg))
;; 	      (t arg))))
;;       (next-line n)
;;       (move-beginning-of-line 1)
;;       (push-mark (point))
;;       (move-end-of-line 1)
;;       (comment-or-uncomment-region (mark) (point)))))

;; (defun comment-line-or-region (arg)
;;   "If the region is active and `transient-mark-mode' is on, call
;; `comment-region' (unless it only consists of comments, in which
;; case it calls `uncomment-region').
;; Else, call `comment-line'."
;;   (interactive "*P")
;;   (comment-normalize-vars)
;;   (if (and mark-active transient-mark-mode)
;;       (comment-or-uncomment-region (region-beginning) (region-end) arg)
;;     (comment-line 0)))


;; ****************************
;; *                          *
;; * COPY BUFFER TO CLIPBOARD *
;; *                          *
;; ****************************

(defun copy-buffer-to-clipboard ()
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))


;; **********************
;; *                    *
;; * JUMP TO BLANK LINE *
;; *                    *
;; **********************
;; Jump to the next blank line, even if paragraphs are redefined
;; TODO: support evil ";" repetition.

(defun jump-to-next-blank-line ()
  (interactive)
  (re-search-forward "\n[\s-]*\n")
  (previous-line))

(defun jump-to-previous-blank-line ()
  (interactive)
  (re-search-backward "\n[\s-]*\n")
  (next-line))


;; *****************************
;; *                           *
;; * FILE MANAGEMENT FUNCTIONS *
;; *                           *
;; *****************************

(defun make-executable ()
  (interactive)
  (shell-command (concat "chmod a+x " buffer-file-name)))


;; ***************
;; *             *
;; * KEY BINDING *
;; *             *
;; ***************

(defun define-key-multi-modes (key def keymaps)
  (dolist (keymap keymaps)
    (define-key (symbol-value keymap) key def)))

;; FIXME: mode-hook verson
;; (defun define-key-multi-modes (key def modes)
;;   (dolist (mode modes)
;;     (add-hook
;;      (concat-symbols mode '-hook)
;;      (lambda nil
;;        (define-key (concat-symbols mode '-map)
;; 	 key def)))))


;; *********
;; *       *
;; * ALIGN *
;; *       *
;; *********

;; from http://emacswiki.org/emacs/AlignCommands
;; GPL2
(defun align-repeat (start end regexp)
  "Repeat alignment with respect to
     the given regular expression."
  (interactive "r\nsAlign regexp: ")
  (align-regexp start end
		(concat "\\(\\s-*\\)" regexp) 1 1 t))
