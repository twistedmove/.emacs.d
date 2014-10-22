;; I created this little snippet in response to this Emacs.SE question:
;; http://emacs.stackexchange.com/a/1039/93

(defun nispio/export-to-parent ()
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
	  (add-hook 'after-save-hook 'nispio/export-to-parent nil t))
	(switch-to-buffer org-buf)
	(kill-buffer buf)))

;; Open the current TSV file as an Org table
(global-set-key (kbd "C-c |") 'nispio/edit-dsv-as-orgtbl)
