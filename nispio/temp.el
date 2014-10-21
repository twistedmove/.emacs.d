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

(require 'thingatpt)

(global-set-key [remap list-buffers] 'ibuffer)

(bind-keys :map matlab-mode-map
		   ("C-i" . previous-line)
		   ("C-k" . next-line)
		   ("C-j" . backward-char)
		   ("C-l" . forward-char))


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

(local-key-binding (format "%c" (? (kbd "C-h")))
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

;; (with-current-buffer (find-file-noselect file)
;;   (setq buf (current-buffer))
;;   (erase-buffer)
;;   (fundamental-mode)
;;   (insert txt "\n")
;;   (save-buffer))
;; (kill-buffer buf)
;; (message "Export done."))
;; (error "TABLE_EXPORT_FORMAT invalid"))

(require 'cl)
(defun org-transpose-table-at-point ()
  "Transpose orgmode table at point, eliminate hlines."
  (interactive)
  (let ((contents (apply #'mapcar* #'list    ;; <== LOB magic imported here
                         (remove-if-not 'listp  ;; remove 'hline from list
                                        (org-table-to-lisp))))  ;; signals error if not table
        )
    (delete-region (org-table-begin) (org-table-end))
    (insert (mapconcat (lambda(x) (concat "| " (mapconcat 'identity x " | " ) " |\n" ))
                       contents
                       ""))
    (org-table-align)
    )
)

;; Make ibuffer auto-update after changes
;; (source: http://emacs.stackexchange.com/a/2179/93)
(defun nispio/ibuffer-stale-p (&optional noconfirm)
  (frame-or-buffer-changed-p 'ibuffer-auto-buffers-changed))
(defun nispio/ibuffer-auto-revert-setup ()
  (set (make-local-variable 'buffer-stale-function)
       'nispio/ibuffer-stale-p)
  (auto-revert-mode 1))
(add-hook 'ibuffer-mode-hook 'nispio/ibuffer-auto-revert-setup)

;; (bind-key (kbd "C-7") 'helm-find-files global-map)
;; (bind-key (kbd "C-8") 'helm-buffers-list global-map)

;; (bind-key (kbd "C-9") 'phi-search-from-isearch-mc/mark-all isearch-mode-map)


(defun phi-search-complete-with-selection ()
  (interactive)
  (let ((query (buffer-string)))
    (phi-search-complete)
    (mc/execute-command-for-all-cursors
     (lambda ()
       (interactive)
       (when (looking-back query)
         (push-mark (match-beginning 0) t t)
         (goto-char (match-end 0))
         (activate-mark))))))
(bind-key (kbd "C-9") 'phi-search-complete-with-selection)


(bind-key (kbd "C-8") helm-command-map (current-global-map))

;; Show the current function name in the header line
(which-function-mode)
(setq-default header-line-format
              '((which-func-mode ("//" which-func-format " "))))
(setq mode-line-misc-info
	  (assq-delete-all 'which-func-mode mode-line-misc-info))
(set-face-attribute 'which-func nil
  :foreground nil
  :background nil
  :inherit font-lock-comment-face)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(which-func ((t (:inherit font-lock-comment-face :foreground nil :background nil)))))


(format "%s" (face-id 'which-func))

(require 'scroll-restore)
(scroll-restore-mode 1)
;; Allow scroll-restore to modify the cursor face
(setq scroll-restore-handle-cursor t)
;; Make the cursor invisible while POINT is off-screen
(setq scroll-restore-cursor-type nil)
;; Jump back to the original cursor position after scrolling
(setq scroll-restore-jump-back t)
;; Toggle scroll-restore-mode with the Scroll Lock key
(global-set-key (kbd "<Scroll_Lock>") 'scroll-restore-mode)


(defun kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(define-key ido-completion-map (kbd "C-l") 'kill-back-to-indentation)

(global-unset-key (kbd "C-8"))
(require 'helm-helm-commands)
(global-set-key (kbd "C-8 C-8") 'helm-helm-commands)



(defun take-that-repeat () (interactive)
  (message "And that! (%d)" (cl-incf my-repeat-count)))

(defun take-that-start ()
  (interactive)
  (message "Take that!")
  (setq my-repeat-count 1)
  (let ((map (make-sparse-keymap)))
	 (define-key map (kbd "=") 'take-that-repeat)
	 (set-temporary-overlay-map map t)))

(define-key my-map (kbd "C-x =") 'take-that-start)

(defun gud-watch (&optional arg event)
  "Watch expression at point.
With arg, enter name of variable to be watched in the minibuffer."
  (interactive (list current-prefix-arg last-input-event))
  (let ((minor-mode (buffer-local-value 'gud-minor-mode gud-comint-buffer)))
    (if (eq minor-mode 'gdbmi)
	(progn
	  (if event (posn-set-point (event-end event)))
	  (require 'tooltip)
	  (save-selected-window
	    (let ((expr
		   (if arg
		       (completing-read "Name of variable: "
					'gud-gdb-complete-command)
		     (if (and transient-mark-mode mark-active)
			 (buffer-substring (region-beginning) (region-end))
		       (concat (if (derived-mode-p 'gdb-registers-mode) "$")
			       (tooltip-identifier-from-point (point)))))))
	      (set-text-properties 0 (length expr) nil expr)
	      (gdb-input (concat "-var-create - * "  expr "")
			 `(lambda () (gdb-var-create-handler ,expr))))))
      (message "gud-watch is a no-op in this mode."))))

(custom-set-faces
 '(mode-line ((t (:box nil))))
 '(mode-line-buffer-id ((t (:foreground "firebrick" :background nil))))
 '(mode-line-inactive ((t (:box nil)))))

 '(mode-line-highlight ((t (:box nil))))
 '(mode-line-inactive ((t (:box nil)))))
