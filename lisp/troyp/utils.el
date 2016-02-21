;; =============================================================================
;;                                 ___________________ 
;;                                |                   |
;;                                | UTILITY FUNCTIONS |
;;                                |___________________|


(fset 'id 'identity)

(defmacro has-value-p (sym)
  "(has-value-p SYM): Returns t if SYM is bound and non-null, nil otherwise"
  `(and (boundp ',sym) ,sym t))

(defun add-multiple-to-list (list &rest elts)
  (dolist (elt elts)
	(add-to-list list elt))
  list)

(defun zap-upto-char (arg char)
  (interactive "p\nczap up to char: ")
  (zap-to-char arg char)
  (insert char)
  (backward-char))

(defun lines (&rest ss)
  "convert a list of string arguments into a string with each entry seperated by newlines"
  (mapconcat 'id ss "\n"))

(defun lines-to-string (ss &optional sep)
  "convert a list of strings into a string with each entry seperated by sep (default: newlines)"
  (mapconcat 'id ss sep))

(defun insert-lines (ss &optional sep)
  "insert a list of strings, separated by sep (default: newline)"
  (unless sep (setf sep "\n"))
  (insert (lines-to-string ss sep)))

;; (defun flatlines (&rest ls)
;;   "convert a tree of strings into a single string, with each entry on a new line"
;;   (if (null ls)
;;       ""
;;     (let ((x  (car ls))
;; 	  (xs (cdr ls)))
;;       (cond
;;        ((stringp x) (lines x (flatlines xs)))
;;        ((listp x)   (apply #'flatlines (append x xs)))))))

(defun flatlines (&rest ls)
  "convert a tree of strings into a list of strings"
  (if (null ls)
      nil
    (let ((x  (car ls))
	  (xs (cdr ls)))
      (cond
       ((stringp x) (cons x (flatlines xs)))
       ((listp x)   (apply #'flatlines (append x xs)))))))

(defun indent-string (s &optional n)
  (unless n (setf n 4))
  (concat (make-string n ?\ ) s))

(defun indent-lines (ss &optional n)
  (unless n (setf n 4))
  (mapcar 
   (lambda (s) (indent-string s n))
   ss))

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

(defmacro with-comment (f &rest args)
  "call f on args and comment the resulting region"
  `(progn
     (push-mark (point))
     (,f ,@args)
     (comment-region (point) (mark))
     (pop-mark)))

(defmacro with-buffer-string (code)
  "Perform operations on temp buffer and return contents as string"
  `(with-temp-buffer 
     ,code
     (buffer-string)))


;; =============================================================================
;;                            _____________________________ 
;;                           |                             |
;;                           | LINE-BASED EDITING COMMANDS |
;;                           |_____________________________|


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
  "Open a new line above the current line, without moving cursor."
  (interactive "p")
  (save-excursion
    (push (point) buffer-undo-list)
    (previous-line 1)
    (move-end-of-line nil)
    (newline n)))

(defun open-line-below (n)
  "Open a new line below the current line, without moving cursor."
  (interactive "p")
  (save-excursion
    (push (point) buffer-undo-list)
    (move-end-of-line nil)
    (open-line n)))


;; ---------------------------------------------------------------------------
;; ,--------------------,
;; | JUMP TO BLANK LINE |
;; '--------------------'

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


;; ---------------------------------------------------------------------------
;; ,---------------------------------,
;; | ECLIPSE-STYLE BEGINNING-OF-LINE |
;; '---------------------------------'

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


;; =============================================================================
;;                                   _______________ 
;;                                  |               |
;;                                  | FILE & BUFFER |
;;                                  |_______________|


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

(defun make-executable ()
  (interactive)
  (shell-command (concat "chmod a+x '" buffer-file-name "'")))

(defun add-subdirs-to-load-path (basedir)
  (add-to-list 'load-path basedir)
  (dolist (subdir-name (directory-files basedir))
    (let ((subdir (concat basedir "/" subdir-name)))
      (when (and (file-directory-p subdir)
                 (not (string-prefix-p "." subdir-name)))
        (add-to-list 'load-path subdir)))))


;; ---------------------------------------------------------------------------
;; ,--------------------------,
;; | COPY BUFFER TO CLIPBOARD |
;; '--------------------------'

(defun copy-buffer-to-clipboard ()
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))


;; =============================================================================
;;                                     __________ 
;;                                    |          |
;;                                    | COMMENTS |
;;                                    |__________|


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


;; =============================================================================
;;                                    _____________ 
;;                                   |             |
;;                                   | KEY BINDING |
;;                                   |_____________|


(defun define-key-multi-modes (key def keymaps)
  (dolist (keymap keymaps)
    (define-key (symbol-value keymap) key def)))

;; FIXME: mode-hook verson
(defun define-key-multi-modes (key def modes)
  (dolist (mode modes)
    (let ((hook-name (concat-symbols mode '-hook))
          (map-name  (concat-symbols mode '-map)))
      (if (boundp map-name)
          (define-key (symbol-value map-name) key def)
        (add-hook hook-name
                  (lambda () (define-key map-name key def)))))))
      


;; =============================================================================
;;                                       _______ 
;;                                      |       |
;;                                      | ALIGN |
;;                                      |_______|

(defun pcre-align (BEG END s &optional group spacing repeat)
  "Align region using a PCRE. Requires pcre2el."
  (interactive "r\nsPCRE (group around part to extend): ")
  (unless BEG (setq BEG (region-beginning)))
  (unless BEG (setq BEG (region-end)))
  (unless group (setq group 1))
  (unless spacing (setq spacing align-default-spacing))
  (align-regexp BEG END (pcre-to-elisp s) group spacing repeat))

(defun quick-pcre-align (BEG END s &optional spacing repeat)
  "Align region using a PCRE. PCRE doesn't require the group for expansion. Requires pcre2el."
  (interactive "r\nsPCRE to align on: ")
  (unless BEG (setq BEG (region-beginning)))
  (unless BEG (setq BEG (region-end)))
  (unless spacing (setq spacing align-default-spacing))
  (let ((regexp (concat "(\s*)" s)))
    (align-regexp BEG END (pcre-to-elisp regexp) 1 spacing repeat)))

(defun quick-align (BEG END s &optional spacing repeat)
  "Align region using a regexp. The regexp doesn't require the group for expansion."
  (interactive "r\nsRegexp to align on: ")
  (unless BEG (setq BEG (region-beginning)))
  (unless BEG (setq BEG (region-end)))
  (unless spacing (setq spacing align-default-spacing))
  (let ((regexp (concat "\\(\\s-*\\)" s)))
    (align-regexp BEG END regexp 1 spacing repeat)))

(defun quick-pcre-align-repeat (BEG END s &optional spacing)
  "Align region using repeated matches of a PCRE. Requires pcre2el."
  (interactive "r\nsPCRE to align on: ")
  (unless BEG (setq BEG (region-beginning)))
  (unless BEG (setq BEG (region-end)))
  (unless spacing (setq spacing align-default-spacing))
  (let ((regexp (concat "(\s*)" s)))
    (align-regexp BEG END (pcre-to-elisp regexp) 1 spacing t)))


;; =============================================================================
;;                                       _______ 
;;                                      |       |
;;                                      | ELISP |
;;                                      |_______|


(defun kill-eval (sxp)
  "Evaluate elisp input and store the pretty-printed result in kill ring."
  (interactive "XELISP>> ")
  (let ((res (pp-to-string sxp)))
    (kill-new res)
    res))

(defun kill-eval-str (sxp)
  "Evaluate elisp input and store the pretty-printed result in kill ring (strings not quoted)."
  (interactive "XELISP>> ")
  (let ((res (if (stringp sxp)
		 sxp
	       (pp-to-string sxp))))
    (kill-new res)
    res))

(defmacro iwrap (CODE)
  "Generate interactive wrapper for CODE"
  `(lambda ()
     (interactive)
     (eval ,CODE)))

(defmacro defiwrap (FNAME CODE)
  "Define function FNAME as interactive wrapper for CODE"
  `(defun ,FNAME ()
     (interactive)
     (eval ,CODE)))


;; =============================================================================
;;                                       _______ 
;;                                      |       |
;;                                      | OTHER |
;;                                      |_______|

(defun buffer-font-set (FONTFAMILY)
  "Set and use temporary face for use in current buffer. Toggle with M-x buffer-face-mode."
  (interactive "sFont family: ")
  (let ((prev-buffer-face-mode-face buffer-face-mode-face))
	(setq buffer-face-mode-face (list :family FONTFAMILY))
	(buffer-face-mode))
	(setq buffer-face-mode-face (list :family prev-buffer-face-mode-face)))

;; modified from Emacs source: GPL3 
(defun shell-command-replace-region
    (START END COMMAND &optional OUTPUT-BUFFER REPLACE ERROR-BUFFER DISPLAY-ERROR-BUFFER)
  "A version of shell-command-on-region which automatically operates on the current buffer"
  (interactive (let (string)
		 (unless (mark)
		   (error "The mark is not set now, so there is no region"))
		 ;; Do this before calling region-beginning
		 ;; and region-end, in case subprocess output
		 ;; relocates them while we are in the minibuffer.
		 (setq string (read-shell-command "Shell command on region: "))
		 ;; call-interactively recognizes region-beginning and
		 ;; region-end specially, leaving them in the history.
		 (list (region-beginning) (region-end)
		       string
               t
               t
		       shell-command-default-error-buffer
		       t)))
  (shell-command-on-region START END COMMAND OUTPUT-BUFFER REPLACE
                           ERROR-BUFFER DISPLAY-ERROR-BUFFER))