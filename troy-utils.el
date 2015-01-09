(fset 'id 'identity)

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


;; ******************************
;; *                            *
;; * HEADING PRINTING FUNCTIONS *
;; *                            *
;; ******************************

(defvar *box-heading-symbol* ?*)
(defvar *box-heading-margin* 1)
(defvar *banner-heading-symbol* ?=)
(defvar *heading-symbol* ?-)

(defun box-heading-string (s)
  (interactive)
  (unless (boundp '*box-heading-symbol*) (setq *box-heading-symbol* "**"))
  (unless (boundp '*box-heading-margin*) (setq *box-heading-margin* 1))
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
	 (h-margin         (concat (string vsym) (make-string (- N 2) spc) (string vsym-right)))
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

(defun box-heading (s)
  (interactive)
  (insert (box-heading-string s)))

(defun box-heading-custom (s syms)
  (interactive)
  (let ((*box-heading-symbol* syms))
    (box-heading s)))

(defun box-heading-comment (s)
  (interactive "s" "heading: ")
  (let ((start (point)))
    (box-heading s)
    (comment-region start (point))))


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
(defun new-line-above (n)
  (interactive "p")
  (save-excursion
    (previous-line 1)
    (move-end-of-line nil)
    (newline n)))
(defun new-line-below (n)
  (interactive "p")
  (save-excursion
    (move-end-of-line nil)
    (newline n)))
(defun insert-line-above (n)
  (interactive "p")
  (beginning-of-line)
  (open-line n)
  (indent-according-to-mode)
  (save-excursion
    (dotimes (i (- n 1) nil)
      (next-line)
      (indent-according-to-mode))))
    
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


; functionality replaced by evil-nerd-commenter
;; ;; ************
;; ;; *          *
;; ;; * COMMENTS *
;; ;; *          *
;; ;; ************

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
