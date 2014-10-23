
(require 'nispio/misc-utils) ;; for nispio/trim-string

(defun nispio/org-table-location (&optional arg)
  "Get the location "
  (interactive "P")
  (when (eq 'org-mode major-mode)
    (org-table-get-specials)
    (let* ((row (org-table-current-dline))
		   (col (org-table-current-column))
		   (loc (if arg
					(format "%c%02d" (+ 64 col) row)
				  (format "@%d$%d" row col))))
      (when (called-interactively-p 'any)
		(message "Field Location: %s" loc))
      loc)))

(defun nispio/org-table-field (&optional arg)
  (interactive "P")
  (when (eq 'org-mode major-mode)
    (org-table-get-specials)
    (let* ((formula (org-table-current-field-formula))
		   (value (nispio/trim-string (org-table-get-field)))
		   (field (or (and arg formula) value)))
      (when (called-interactively-p 'any)
		(message "Field Value: %s" loc))
      field)))

;; Define the format for the header line in Org mode
(setq nispio/org-table-header
	  (list '(:eval (let ((loc (nispio/org-table-location))
						  (field (nispio/org-table-field)))
					  (format " %s: %s" loc field)))))

;; I created this little snippet in response to my own Emacs.SE question:
;; http://emacs.stackexchange.com/q/774/93
(defun nispio/org-mode-setup ()
  "Apply custom setup to org-mode buffers"
  (setq-local header-line-format nispio/org-table-header))
(add-hook 'org-mode-hook 'nispio/org-mode-setup)


;; This function is meant to replace org-table-align to change the way that
;; truncation works for right-aligned columns. It was provided as a response to
;; the following Emacs.SE question:
;; http://emacs.stackexchange.com/a/746/93
(eval-after-load "org-table"
  (progn
    (defun nispio/org-table-align ()
      "Align the table at point by aligning all vertical bars. This function has
 been modified to change the way that truncation works when a column
 right-aligned vs left-aligned.

 source: http://emacs.stackexchange.com/a/746/93"
     (interactive)
     (let* (
	    ;; Limits of table
	    (beg (org-table-begin))
	    (end (org-table-end))
	    ;; Current cursor position
	    (linepos (org-current-line))
	    (colpos (org-table-current-column))
	    (winstart (window-start))
	    (winstartline (org-current-line (min winstart (1- (point-max)))))
	    lines (new "") lengths l typenums ty fields maxfields i
	    column
	    (indent "") cnt frac
	    rfmt hfmt
	    (spaces '(1 . 1))
	    (sp1 (car spaces))
	    (sp2 (cdr spaces))
	    (rfmt1 (concat
		    (make-string sp2 ?\ ) "%%%s%ds" (make-string sp1 ?\ ) "|"))
	    (hfmt1 (concat
		    (make-string sp2 ?-) "%s" (make-string sp1 ?-) "+"))
	    emptystrings links dates emph raise narrow
	    falign falign1 fmax f1 len c e space)
       (untabify beg end)
       (remove-text-properties beg end '(org-cwidth t org-dwidth t display t))
       ;; Check if we have links or dates
       (goto-char beg)
       (setq links (re-search-forward org-bracket-link-regexp end t))
       (goto-char beg)
       (setq emph (and org-hide-emphasis-markers
		       (re-search-forward org-emph-re end t)))
       (goto-char beg)
       (setq raise (and org-use-sub-superscripts
			(re-search-forward org-match-substring-regexp end t)))
       (goto-char beg)
       (setq dates (and org-display-custom-times
			(re-search-forward org-ts-regexp-both end t)))
       ;; Make sure the link properties are right
       (when links (goto-char beg) (while (org-activate-bracket-links end)))
       ;; Make sure the date properties are right
       (when dates (goto-char beg) (while (org-activate-dates end)))
       (when emph (goto-char beg) (while (org-do-emphasis-faces end)))
       (when raise (goto-char beg) (while (org-raise-scripts end)))

       ;; Check if we are narrowing any columns
       (goto-char beg)
       (setq narrow (and org-table-do-narrow
			 org-format-transports-properties-p
			 (re-search-forward "<[lrc]?[0-9]+>" end t)))
       (goto-char beg)
       (setq falign (re-search-forward "<[lrc][0-9]*>" end t))
       (goto-char beg)
       ;; Get the rows
       (setq lines (org-split-string
		    (buffer-substring beg end) "\n"))
       ;; Store the indentation of the first line
       (if (string-match "^ *" (car lines))
	   (setq indent (make-string (- (match-end 0) (match-beginning 0)) ?\ )))
       ;; Mark the hlines by setting the corresponding element to nil
       ;; At the same time, we remove trailing space.
       (setq lines (mapcar (lambda (l)
			     (if (string-match "^ *|-" l)
				 nil
			       (if (string-match "[ \t]+$" l)
				   (substring l 0 (match-beginning 0))
				 l)))
			   lines))
       ;; Get the data fields by splitting the lines.
       (setq fields (mapcar
		     (lambda (l)
		       (org-split-string l " *| *"))
		     (delq nil (copy-sequence lines))))
       ;; How many fields in the longest line?
       (condition-case nil
	   (setq maxfields (apply 'max (mapcar 'length fields)))
	 (error
	  (kill-region beg end)
	  (org-table-create org-table-default-size)
	  (error "Empty table - created default table")))
       ;; A list of empty strings to fill any short rows on output
       (setq emptystrings (make-list maxfields ""))
       ;; Check for special formatting.
       (setq i -1)
       (while (< (setq i (1+ i)) maxfields)   ;; Loop over all columns
	 (setq column (mapcar (lambda (x) (or (nth i x) "")) fields))
	 ;; Check if there is an explicit width specified
	 (setq fmax nil)
	 (when (or narrow falign)
	   (setq c column fmax nil falign1 nil)
	   (while c
	     (setq e (pop c))
	     (when (and (stringp e) (string-match "^<\\([lrc]\\)?\\([0-9]+\\)?>$" e))
	       (if (match-end 1) (setq falign1 (match-string 1 e)))
	       (if (and org-table-do-narrow (match-end 2))
		   (setq fmax (string-to-number (match-string 2 e)) c nil))))
	   ;; Find fields that are wider than fmax, and shorten them
	   (when fmax
	     (loop for xx in column do
		   (when (and (stringp xx)
			      (> (org-string-width xx) fmax))
		     (org-add-props xx nil
		       'help-echo
		       (concat "Clipped table field, use C-c ` to edit.  Full value is:\n" (org-no-properties (copy-sequence xx))))
		     (setq f1 (min fmax (or (string-match org-bracket-link-regexp xx) fmax)))
		     (unless (> f1 1)
		       (error "Cannot narrow field starting with wide link \"%s\""
			      (match-string 0 xx)))
		     ;;
		     ;; MODIFICATION BEGIN
		     ;;
		     ;; (add-text-properties f1 (length xx) (list 'org-cwidth t) xx)
		     ;; (add-text-properties (- f1 2) f1
		     ;;                (list 'display org-narrow-column-arrow)
		     ;;                xx)))))
		     (let (s1 e1 s2 e2 arrow-string)
		       (if (and falign1 (equal (downcase falign1) "r"))
			   (setq s1 0
				 e1 (- (length xx) f1)
				 s2 (- (length xx) f1)
				 e2 (- (length xx) (- f1 2))
				 arrow-string "<=")
			 (setq s1 f1
			       e1 (length xx)
			       s2 (- f1 2)
			       e2 f1
			       arrow-string org-narrow-column-arrow))
		       (add-text-properties s1 e1 (list 'org-cwidth t) xx)
		       (add-text-properties s2 e2 (list 'display arrow-string) xx))))))
                     ;;
                     ;; END MODIFICATION
                     ;;
	   ;; Get the maximum width for each column
	   (push (apply 'max (or fmax 1) 1 (mapcar 'org-string-width column))
		 lengths)
	   ;; Get the fraction of numbers, to decide about alignment of the column
	   (if falign1
	       (push (equal (downcase falign1) "r") typenums)
	     (setq cnt 0 frac 0.0)
	     (loop for x in column do
		   (if (equal x "")
		       nil
		     (setq frac ( / (+ (* frac cnt)
				       (if (string-match org-table-number-regexp x) 1 0))
				    (setq cnt (1+ cnt))))))
	     (push (>= frac org-table-number-fraction) typenums)))
	 (setq lengths (nreverse lengths) typenums (nreverse typenums))

	 ;; Store the alignment of this table, for later editing of single fields
	 (setq org-table-last-alignment typenums
	       org-table-last-column-widths lengths)

	 ;; With invisible characters, `format' does not get the field width right
	 ;; So we need to make these fields wide by hand.
	 (when (or links emph raise)
	   (loop for i from 0 upto (1- maxfields) do
		 (setq len (nth i lengths))
		 (loop for j from 0 upto (1- (length fields)) do
		       (setq c (nthcdr i (car (nthcdr j fields))))
		       (if (and (stringp (car c))
				(or (text-property-any 0 (length (car c))
						       'invisible 'org-link (car c))
				    (text-property-any 0 (length (car c))
						       'org-dwidth t (car c)))
				(< (org-string-width (car c)) len))
			   (progn
			     (setq space (make-string (- len (org-string-width (car c))) ?\ ))
			     (setcar c (if (nth i typenums)
					   (concat space (car c))
					 (concat (car c) space))))))))

	 ;; Compute the formats needed for output of the table
	 (setq rfmt (concat indent "|") hfmt (concat indent "|"))
	 (while (setq l (pop lengths))
	   (setq ty (if (pop typenums) "" "-")) ; number types flushright
	   (setq rfmt (concat rfmt (format rfmt1 ty l))
		 hfmt (concat hfmt (format hfmt1 (make-string l ?-)))))
	 (setq rfmt (concat rfmt "\n")
	       hfmt (concat (substring hfmt 0 -1) "|\n"))

	 (setq new (mapconcat
		    (lambda (l)
		      (if l (apply 'format rfmt
				   (append (pop fields) emptystrings))
			hfmt))
		    lines ""))
	 (move-marker org-table-aligned-begin-marker (point))
	 (insert new)
	 ;; Replace the old one
	 (delete-region (point) end)
	 (move-marker end nil)
	 (move-marker org-table-aligned-end-marker (point))
	 (when (and orgtbl-mode (not (derived-mode-p 'org-mode)))
	   (goto-char org-table-aligned-begin-marker)
	   (while (org-hide-wide-columns org-table-aligned-end-marker)))
	 ;; Try to move to the old location
	 (org-goto-line winstartline)
	 (setq winstart (point-at-bol))
	 (org-goto-line linepos)
	 (set-window-start (selected-window) winstart 'noforce)
	 (org-table-goto-column colpos)
	 (and org-table-overlay-coordinates (org-table-overlay-coordinates))
	 (setq org-table-may-need-update nil)
	 ))
    (defalias 'org-table-align 'nispio/org-table-align)
    nil))


;; I created this little snippet in response to this Emacs.SE question:
;; http://emacs.stackexchange.com/a/1039/93

(defun nispio/export-orgtbl-to-parent ()
  "Exports the table in the current buffer back to its parent DSV file and
    then closes this buffer."
  (let ((buf (current-buffer)))
	(org-table-export parent-file export-func)
	(set-buffer-modified-p nil)
	(switch-to-buffer (find-file parent-file))
	(kill-buffer buf)))

(defun nispio/edit-dsv-as-orgtbl (&optional arg)
  "Convet the current DSV buffer into an org table in a separate file. Saving
    the table will convert it back to DSV and jump back to the original file"
  (interactive "P")
  (let* ((buf (current-buffer))
		 (file (buffer-file-name buf))
		 (txt (substring-no-properties (buffer-string)))
		 (org-buf (find-file-noselect (concat (buffer-name) ".org"))))
	(save-buffer)
	(with-current-buffer org-buf
	  (erase-buffer)
	  (insert txt)
	  (org-table-convert-region 1 (buffer-end 1) arg)
	  (setq-local parent-file file)
	  (cond 
	   ((equal arg '(4)) (setq-local export-func "orgtbl-to-csv"))
	   ((equal arg '(16)) (setq-local export-func "orgtbl-to-tsv"))
	   (t (setq-local export-func "orgtbl-to-csv")))
	  (add-hook 'after-save-hook 'nispio/export-orgtbl-to-parent nil t))
	(switch-to-buffer org-buf)
	(kill-buffer buf)))

(provide 'nispio/org-table-utils)
