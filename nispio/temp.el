;; (defun nispio/show-trimmed-help-string ()
;;   (interactive)
;;   (let* ((help (help-at-pt-string))
;; 	 (str (cdr (split-string (or help "") "\n")))
;; 	 (msg (mapconcat 'identity str "\n")))
;;     (if (called-interactively-p 'any)
;; 	(message msg))
;;     msg))

;; (setq-default header-line-format
;; 		(list 
;; 		 '(:eval (format "%c" (+ 64 (org-table-current-column))))
;; 		 '(:eval (format "%d" (org-table-current-dline)))
;; 		 ": "
;; 		 '(:eval (or (progn (org-table-get-specials)
;; 				    (org-table-current-field-formula))
;; 			     (my-trim-string (org-table-get-field))))
;; 		 ))

(bind-keys ([remap forward-word] . forward-sexp)
		   ([remap backward-kill-word] . backward-kill-sexp)
		   ([remap backward-word] . backward-sexp)
		   ([remap forward-word] . forward-sexp)
		   ([remap kill-word] . kill-sexp)
		   ([remap mark-word] . mark-sexp)
		   ([remap transpose-words] . transpose-sexps)
		   )

(global-set-key [remap list-buffers] 'ibuffer)

(bind-keys :map matlab-mode-map
		   ("C-i" . previous-line)
		   ("C-k" . next-line)
		   ("C-j" . backward-char)
		   ("C-l" . forward-char))

It seems that there

    (defun my-magical-keys ()
      (local-set-key (kbd "C-i") 'previous-line)
      (local-set-key (kbd "C-k") 'next-line)
      (local-set-key (kbd "C-j") 'backward-char)
      (local-set-key (kbd "C-l") 'forward-char))
    (add-hook 'my-magical-mode-hook 'my-matlab-keys)
    
    (define-key my-magical-mode-map (kbd "C-i") 'previous-line)
    (define-key my-magical-mode-map (kbd "C-k") 'next-line)
    (define-key my-magical-mode-map (kbd "C-j") 'backward-char)
    (define-key my-magical-mode-map (kbd "C-l") 'forward-char)

(setq matlab-mode-map (make-sparse-keymap))

(global-set-key "i" 'self-insert-command)		   
(global-set-key "j" 'self-insert-command)		   
(global-set-key "k" 'self-insert-command)		   
(global-set-key "l" 'self-insert-command)		   

(progn

  (defun my-trim-string (arg) 
	"Simple function for trimming the whitespace from the ends of
 a string. Also removes any string properties such as font faces."
	(let ((str (substring-no-properties arg)))
	  (when (string-match "^[ \t]+" str)
		(setq str (replace-match "" nil nil str)))
	  (when (string-match "[ \t]+$" str)
		(setq str (replace-match "" nil nil str)))
	  str))

)

;; (bind-key "C-h C-." 'nispio/show-trimmed-help-string)

;; Show a list of all 
;; (source: http://emacs.stackexchange.com/a/654/93)
(defun nispio/locate-key-binding (key)
  "Determine in which keymap KEY is defined."
  (interactive "kPress key: ")
  (let ((ret (list (nispio/key-binding-at-point key)
		   (minor-mode-key-binding key)
		   (local-key-binding key)
		   (global-key-binding key))))
    (when (called-interactively-p 'any)
      (with-output-to-org-buffer "*locate-key*"
	;; (split-window-vertically -4)
	(princ (format "Key Bindings for %s\n\n" (key-description key)))
	(princ (format "At Point: %s\n" (or (nth 0 ret) "nil")))
	(princ (format "Minor-mode: %s\n"
		       (or (and (nth 1 ret)
				(mapconcat
				 (lambda (x) (format "%s: %s" (car x) (cdr x)))
				 (nth 1 ret) "\n            ")) "nil")))
	(princ (format "Local: %s\n" (or (nth 2 ret) "nil")))
	(princ (format "Global: %s" (or (nth 3 ret) "nil")))))
    ret))
(bind-key "C-h k" 'nispio/locate-key-binding)

(local-key-binding (format "%c" (? (kbd "C-h")) )
(lookup-key (current-local-map) (kbd "C-h k"))
(local-set-key (kbd "C-M-j") 'eval-print-last-sexp)

(nispio/locate-key-binding (kbd "C-h C-k"))
(prin1-to-string (local-key-binding (kbd "C-h C-k"))

(let* ((key (kbd "C-h k"))
       (desc (key-description key))
       (function (key-binding key))
       (arglist (help-function-arglist function t))
       (usage (help-make-usage function arglist))
       (doc (documentation function))
       (at-point-binding (nispio/key-binding-at-point key))
       (minor-mode-binding (minor-mode-key-binding key))
       (local-binding (local-key-binding key))
       (global-binding (global-key-binding key))
       )
  (with-help-window (help-buffer)
    (save-excursion
    (read-only-mode -1)
    (princ (format "Key Bindings for %s\n\n" desc))
    (when at-point-binding
      (princ (format "At Point: %S\n" at-point-binding)))
    (when minor-mode-binding
      (princ (format "Minor-mode: %s\n"
        (mapconcat (lambda (x) (format "%s: %s" (car x) (cdr x)))
		   minor-mode-binding "\n            "))))
    (when (and local-binding (not (numberp local-binding)))
      (princ (format "Local: %s\n" local-binding)))
    (when global-binding
      (princ (format "Global: %s\n" global-binding)))
    ;(princ (format "\n%s\n\n%s" usage doc))
    ))
  nil)

(setq test-dummy t)
(key-binding (kbd "C-h k"))
(key-description (kbd "C-h k"))

    (defun my-export-to-parent ()
      "Exports the table in the current buffer back to its parent TSV file and
    then closes this buffer."
      (unless (org-at-table-p)
        (error "No table at point"))
      (require 'org-exp)
      (org-table-align)
      (let ((buf (current-buffer))
            (file parent-file)
            (fmt "orgtbl-to-tsv"))
        (org-table-export file "orgtbl-to-tsv")
        (set-buffer-modified-p nil)
        (switch-to-buffer (find-file-noselect file))
        (kill-buffer buf)))
    
    (defun my-edit-tsv-as-orgtbl ()
      "Convet the current TSV buffer into an org table in a separate file. Saving
    the table will convert it back to TSV and jump back to the original file"
      (interactive)
      (let* ((buf (current-buffer))
             (file (buffer-file-name buf))
             (beg (buffer-end -1))
             (end (buffer-end 1))
             (txt (buffer-substring-no-properties beg end))
             (org-buf (find-file-noselect (concat (buffer-name) ".org"))))
        (save-buffer buf)
        (with-current-buffer org-buf
          (erase-buffer)
          (insert txt "\n")
          (org-table-convert-region (buffer-end -1) (buffer-end 1) '(16))
          (setq-local parent-file file)
          (add-hook 'after-save-hook 'my-export-to-parent nil t))
        (switch-to-buffer org-buf)
        (kill-buffer buf)))
    
    ;; Open the current TSV file as an org table
    (global-set-key (kbd "C-c |") 'my-edit-tsv-as-orgtbl)   
    
	  (with-current-buffer (find-file-noselect file)
	    (setq buf (current-buffer))
	    (erase-buffer)
	    (fundamental-mode)
	    (insert txt "\n")
	    (save-buffer))
	  (kill-buffer buf)
	  (message "Export done."))
      (error "TABLE_EXPORT_FORMAT invalid"))))

  
)
