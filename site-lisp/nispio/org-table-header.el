;; I created this little snippet in response to my own Emacs.SE question:
;; http://emacs.stackexchange.com/q/774/93

(unless (fboundp 'nispio/trim-string)
  (defun nispio/trim-string (arg) 
	"Simple function for trimming the whitespace from the ends of
 a string. Also removes any string properties such as font faces."
	(let ((str (substring-no-properties arg)))
	  (when (string-match "^[ \t]+" str)
		(setq str (replace-match "" nil nil str)))
	  (when (string-match "[ \t]+$" str)
		(setq str (replace-match "" nil nil str)))
	  str)))

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

(defun nispio/org-mode-setup ()
  "Apply custom setup to org-mode buffers"
  (setq-local header-line-format nispio/org-table-header))
(add-hook 'org-mode-hook 'nispio/org-mode-setup)
