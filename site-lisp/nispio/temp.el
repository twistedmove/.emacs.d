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

(defun my-repeatable-command ()
  (interactive)
  (message "Now press TAB to repeat")
  (setq my-repeat-count 0)
  (let ((map (make-sparse-keymap)))
	 (define-key map (kbd "<tab>") 'my-forward-command)
	 (define-key map (kbd "<backtab>") 'my-backward-command)
	 (define-key map (kbd "<return>") 'my-end-repeatable-command)
	 (set-transient-map map t)))

(defun my-forward-command () (interactive)
  (setq my-repeat-count (+ 1 my-repeat-count))
  (message "Number is %d" my-repeat-count))

(defun my-backward-command () (interactive)
  (setq my-repeat-count (+ -1 my-repeat-count))
  (message "Number is %d" my-repeat-count))

(defun my-end-repeatable-command () (interactive)
  (message "You ended on %d" my-repeat-count))

(global-set-key (kbd "<C-tab>") 'my-repeatable-command)

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


;; If not in a TTY, Unbind C-m so that we can use it elsewhere
(if (not (display-graphic-p))
    (setq tty-keys t)
  (define-key input-decode-map [?\C-m] [C-m])
  (define-key local-function-key-map [C-m] [?\C-m])
  (define-key input-decode-map [?\C-i] [C-i])
  (define-key input-decode-map [?\C-\[] [C-\[])
  (setq tty-keys nil))

(define-key emacs-lisp-mode-map [C-m] 'newline-and-indent)

(define-key input-decode-map [?\C-m] (kbd "<C-m>"))


(listify-key-sequence (kbd "C-c ESC"))
(listify-key-sequence [?\C-m])
(listify-key-sequence [C-m])
(listify-key-sequence (kbd "<C-m>"))

(defun nispio/fake-M-RET ()
  (interactive)
  (let ((command (key-binding (kbd "<M-return>"))))
  	(setq last-command-event [M-return])
  	(setq this-command command)
  	(call-interactively command)))
(add-hook 'org-mode-hook (lambda () (local-set-key (kbd "<C-m>") 'nispio/fake-M-RET))))


(defmacro my-fake-keypress (key)
  "Simulate a keypress event"
  (interactive)
  `(let ((command (key-binding ,key)))
	 (nconc unread-command-events ,key)
	 (setq last-command-event ,key)
	 (setq this-command command)
	 (call-interactively command)
	 (message "%s" command)))


(key-binding (kbd "M-g"))

(insert (nispio/describe-keymap (key-binding (kbd "M-g"))))

(keymap (9 . move-to-column) (112 . previous-error) (110 . next-error) (27 keymap (112 . previous-error) (110 . next-error) (103 . goto-line)) (103 . goto-line) (99 . goto-char))


(keymap (keymap (119 . phi-rectangle-kill-ring-save) (111 keymap (54 keymap (98 . enlarge-window) (97 . shrink-window) (99 . enlarge-window-horizontally) (100 . shrink-window-horizontally))) (115 keymap (114 . helm-register) (111 . helm-occur) (97 . helm-do-grep) (98 . nispio/helm-moccur-buffers) (110 . find-name-dired)) (49 . delete-other-windows) (48 . delete-window) (67108902 . disable-my-global-mode)) (keymap (keymap (17 . indent-pp-sexp) (24 . eval-defun) (9 . completion-at-point)) keymap (keymap (17 . indent-sexp)) keymap (17 . prog-indent-sexp)) ESC-prefix)

(substitute-command-keys (format "%s" (key-binding (kbd "M-g"))))


(keymap (32 . sr-speedbar-toggle))




(global-set-key (kbd "C-3") 'ESC-prefix)
(global-set-key (kbd "<C-m>") (lambda () (interactive) (my-fake-keypress (kbd "T"))))

(defun nispio/press-esc () (interactive)
	(nconc unread-command-events (kbd "Q")))

(add-to-list 'jo-mama (kbd "ESC"))
(add-to-list 'jo-mama (kbd "C-i"))

(setq list1 '(alpha beta gamma))
(setq list2 '(delta epsilon theta))
(nconc nil '(iota))
(append list1 list2)

input-decode-map


(keymapp '(keymap (13 . [C-m]) (27 keymap (C-backspace) (C-delete))` (C-M-backspace) (C-M-delete) (M-backspace) (M-delete)))

(global-set-key (kbd "C-`") esc-map)


(defun nispio/describe-keymap (keymap)
  "List the binding in KEYMAP in a human-readable format"
  (interactive
   (list (intern (completing-read "Keymap: " obarray
     (lambda (m) (and (boundp m) (keymapp (symbol-value m)))) t nil nil))))
  (unless (and (symbolp keymap) (boundp keymap) (keymapp (symbol-value keymap)))
    (error "`%S' is not a keymapp" keymap))
  (let ((name (symbol-name keymap)))
	(with-help-window (help-buffer)
	  (save-excursion
		(read-only-mode -1)
		(princ (format "Key bindings in keymap `%s':\n\n" name))
		(princ (substitute-command-keys (concat "\\{" name "}")))
		))))



(let* ((map (key-binding (kbd "M-g")))
	   (name "the-map")
	   )
  (setq nispio/temp-map map)
  (with-help-window (help-buffer)
  	(save-excursion
  	  (read-only-mode -1)
  	  (princ (format "Key bindings in keymap `%s':\n\n" name))
  	  (princ (substitute-command-keys (concat "\\{nispio/temp-map}")))
  	  ))
)

(defun nispio/simulate-esc ()
  (interactive)
  (let ((keys (listify-key-sequence (this-command-keys)))
		(esc (listify-key-sequence (kbd "ESC"))))
	(nbutlast keys)
	(nconc keys esc)
	(insert (format "Prefix: %s (%s)" keys (key-description keys)))
	(setq the-keys keys)
	(setq unread-command-events keys)))

(global-set-key (kbd "C-`") 'nispio/simulate-esc)
(global-set-key (kbd "M-g C-c C-`") 'nispio/simulate-esc)
(global-set-key (kbd "C-c C-2 C-3 C-4 C-5") 'nispio/simulate-esc)

(setq list1 '(A B C D E F G))
(nbutlast list1)


Prefix: (3 67108914 67108915 67108916 27) (C-c C-2 C-3 C-4 ESC)

(global-set-key (kbd "C-c C-x `") 'nispio/simulate-esc)
(global-set-key (kbd "C-c C-x M-g") 'view-hello-file)

Prefix: (3 24 27) (C-c C-x ESC)

Prefix: (3 24 27) (C-c C-x ESC)


(defmacro simulate-key-event (event &optional N)
  `(let ((prefix (listify-key-sequence (this-command-keys)))
		 (key (listify-key-sequence ,event))
		 (n (prefix-numeric-value ,N)))
	 (if (< n 0)
		 (setq prefix key)
	   (nbutlast prefix n)
	   (nconc prefix key))
	 (setq unread-command-events prefix)))


(defun nispio/simulate-key-event (event &optional N)
  "Simulate an arbitrary keypress event.

This function sets the `unread-command-events' variable in order to simulate a
series of key events given by EVENT. Can also For negative N, simulate the
specified key EVENT directly.  For positive N, removes the last N elements from
the list of key events in `this-command-keys' and then appends EVENT.  For N nil,
treat as N=1."
  (let ((prefix (listify-key-sequence (this-command-keys)))
		 (key (listify-key-sequence event))
		 (n (prefix-numeric-value N)))
	 (if (< n 0)
		 (setq prefix key)
	   (nbutlast prefix n)
	   (nconc prefix key))
	   (setq unread-command-events prefix)))


(defun simulate-esc ()
  (interactive)
  (simulate-key-event (kbd "ESC")))

(defun simulate-Cxh ()
  (interactive)
  (simulate-key-event (kbd "C-x h")))

(defun simulate-C-c-caret ()
  (interactive)
  (my-simulate-key-event (kbd "C-c"))
  (my-simulate-key-event (kbd "^") -1))

(defun simulate-C-c ()
  (interactive)
  (my-simulate-key-event (kbd "C-c")))

(define-key ctl-x-5-map (kbd "M-h") 'view-hello-file)
(define-key ctl-x-5-map (kbd "C-`") 'simulate-esc)

(define-key ctl-x-r-map (kbd "C-x h") 'view-hello-file)
(define-key ctl-x-r-map (kbd "C-`") 'simulate-Cxh)

(define-key ctl-x-4-map (kbd "C-c ^ T") 'view-hello-file)
(define-key ctl-x-4-map (kbd "C-`") 'simulate-C-c-caret)
(define-key ctl-x-4-map (kbd "C-f") 'simulate-C-c)

(setq list1 '(alpha beta delta gamma epsilon))
(nbutlast list1 99)

(global-set-key (kbd "C-`") (kbd "<escape>"))


;; Lisp specific defuns
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
	  (prin1 (eval (read (current-kill 0)))
			 (current-buffer))
	(error (message "Invalid expression")
		   (insert (current-kill 0)))))

(define-key input-decode-map [menu] )
(global-set-key (kbd "C-c C-x H-t") 'view-hello-file)

(global-unset-key [menu])
(define-key local-function-key-map [menu] 'menu-bar-open)

;(global-set-key (kbd "C-]") 'abort-recursive-edit)

(global-set-key (kbd "H-C-]") (global-key-binding (kbd "C-]")))
(global-unset-key (kbd "C-]"))
(define-key local-function-key-map (kbd "C-]") 'hyperize)


(defun hyperize (prompt)
  (let ((e (read-event)))
	(vector (if (numberp e)
				(logior (lsh 1 24) e)
			  (if (memq 'hyper (event-modifiers e))
				  e
				(add-event-modifier "H-" e))))))

(defun superize (prompt)
  (let ((e (read-event)))
	(vector (if (numberp e)
				(logior (lsh 1 24) e)
			  (if (memq 'hyper (event-modifiers e))
				  e
				(add-event-modifier "s-" e))))))

(defun add-event-modifier (string e)
  (let ((symbol (if (symbolp e) e (car e))))
	(setq symbol (intern (concat string
								 (symbol-name symbol))))
	(if (symbolp e)
		symbol
	  (cons symbol (cdr e)))))

(define-key local-function-key-map "\C-ch" 'hyperify)

(let ((prefix-list '("C-M-" "M-" "C-"))
	  (digit-list (number-sequence 0 9))
	  digit-string key-string)
  (dolist (digit digit-list)
	(setq digit-string (format "%d" digit))
	(mapcar
	 (lambda (prefix-string)
	   (setq key-string (concat prefix-string digit-string))
	   ;(message "(global-unset-key (kbd \"%s\"))" key-string)
	   (global-unset-key (kbd key-string)))
	 prefix-list)
	))

(defun nispio/simulate-super ()
  (interactive)
  (setq unread-command-events '(24 64 115)))

(defun nispio/simulate-hyper ()
  (interactive)
  (setq unread-command-events '(24 64 104)))

(defun nispio/simulate-meta ()
  (interactive)
  (setq unread-command-events '(24 64 109)))

(defun nispio/simulate-control ()
  (interactive)
  (setq unread-command-events '(24 64 99)))

(defun nispio/simulate-alt ()
  (interactive)
  (setq unread-command-events '(24 64 97)))

(defun event-apply-meta-modifier (_ignore-prompt)
  "\\<function-key-map>Add the Meta modifier to the following event.
For example, type \\[event-apply-meta-modifier] & to enter Meta-&."
  (vector (event-apply-modifier (read-event) 'meta 27 "M-")))

	
(nispio/directory-subdirs "~/.emacs.d/site-lisp" '(".git"))

(setq list1 '("1" "2" "3" "." ".."))
(append '(".git" ".hg") list1)
	
			  (insert "\n" file)
			  (unless (or (string= "." (substring file -1))
						  (not (file-directory-p el)))
				)
			  
			files
			))
		 ;; remove '.', '..' and all non-directories
		 (filtered-files (remove-if (lambda (el)
									  (or (string= "." (substring el -1))
										  (not (file-directory-p el))))
									all-files)))
	filtered-files))

(prune-directory-list '("~/.emacs.d" "~/.emacs.d/init.el" "~/.emacs.d/site-lisp"))


(nispio/set-buffer-window-height "dev-utils.el" 15)



(mapconcat (lambda (x) (format "%S" (car x))) (cdr gud-tool-bar-map) "\n")
(assoc 'go (cdr gud-tool-bar-map))
(assoc 'GDB (cdr tool-bar-map))

(find-image '((:type xpm :file "attach.xpm")))

(define-key-after tool-bar-map [GDB]
  '(menu-item "GDB" gdb-init-buffer
			  :visible (and (boundp 'gud-comint-buffer)
							(buffer-live-p gud-comint-buffer))
			  :image (image :type xpm :file "attach.xpm")
			  :help "Switch to GDB toolbar in current buffer"))



(setq projectile-require-project-root t)

(eval-after-load "projectile"
  (progn 
	;; (source: https://github.com/bbatsov/projectile/issues/364#issuecomment-61296248)
	(defun projectile-root-child-of (dir &optional list)
	  (projectile-locate-dominating-file
	   dir
	   (lambda (dir)
		 (--first
		  (if (and
			   (s-equals? (file-remote-p it) (file-remote-p dir))
			   (string-match-p (expand-file-name it) (expand-file-name dir)))
			  dir)
		  (or list project-root-regexps (list))))))
	(defvar project-root-regexps ()
	  "List of regexps to match against when projectile is searching
    for project root directories.")
	(nconc projectile-project-root-files-functions '(projectile-root-child-of))
	nil))




(defun nispio/add-killed-rectangle-to-kill-ring ()
  (interactive)
  (when killed-rectangle
	(kill-new killed-rectangle)
	(setq phi-rectangle--last-killed-is-rectangle nil)))
(define-key my-map (kbd "C-c C-M-w") 'nispio/add-killed-rectangle-to-kill-ring)



(defun nispio/shell-command (command &optional name dir)
  (interactive "sCommand: ")
  (let* ((program shell-file-name)
		 (cmd command)
		 (name (or name (car (split-string cmd))))
		 (outbuf (get-buffer-create (concat "*" name "*")))
		 (dir (or dir default-directory))
		 (timestamp (substring (current-time-string) 0 19)))
	(with-current-buffer outbuf
	  (setq-local default-directory dir)
	  (buffer-disable-undo)
	  (let ((inhibit-read-only t))
		(insert (format "\n%s started at %s\n\n%s> %s\n"
						program timestamp (directory-file-name dir) cmd)))
	  (set-buffer-modified-p nil))
	(display-buffer outbuf '(nil (allow-no-window . t)))
	(make-comint-in-buffer name outbuf shell-file-name nil "-c" cmd)
	))

(defun nispio/run-xmidas ()
  (interactive)
  (let ((cmd "xmstart ; eval xm")
		(name "X-Midas")
		(dir "/home/jph/xmidas/"))
	(nispio/shell-command cmd name dir)
	(select-window (get-buffer-window (concat "*" name "*")))))

(defvar my-pylint-history nil)

(defun my-run-pylint (&optional arg)
  (interactive "P")
  (let* ((cmd (concat "pylint " buffer-file-name)))
	(when arg
	  (setq cmd (read-string "Command: " cmd 'my-pylint-history)))
	(shell-command cmd (get-buffer-create "*pylint*"))))



	(when (load "flymake" t)
	  (defun flymake-pylint-init ()
		(let* ((temp-file (flymake-init-create-temp-buffer-copy
						   'flymake-create-temp-inplace))
			   (local-file (file-relative-name
							temp-file
							(file-name-directory buffer-file-name))))
		  (list "epylint" (list local-file))))

	  (add-to-list 'flymake-allowed-file-name-masks
				   '("\\.py\\'" flymake-pylint-init)))



;; code checking via flymake
(setq pycodechecker "epylint.py")
(when (load "flymake" t)
  (defun flymake-pycodecheck-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list pycodechecker (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pycodecheck-init)))



(defvar find-exts "c cc h hh py m el"
  "Last arguments given to `find' by \\[nispio/dired-find-exts].")

(defvar find-exts-history nil)

(defun nispio/dired-find-exts (dir exts)
  (interactive (list (read-directory-name "Run find in directory: " nil "" t)
					 (read-string "Find extensions (no . or *): " find-exts
								  '(find-exts-history . 1))))
  (let* ((dir (file-name-as-directory (expand-file-name dir)))
		 (ext-list (split-string exts))
		 (ext-glob (lambda (x) (concat "\\*." x)))
		 (find-args (mapconcat ext-glob ext-list " -o -name "))
		 (find-args (concat "\\( -path \\*/.git -prune -o \\( -name " find-args " \\) \\) -a -type f")))
	;; Store this time's input for next time
	(setq find-exts exts)
	;; Call `find-dired' to do the rest of the work
	(find-dired dir find-args)))

(define-key my-map (kbd "M-s N") 'nispio/dired-find-exts)

;; (setq find-exts "c cc h hh py m el")


(require 'hideshowvis)




(defun org-capture (&optional goto keys)
  "Capture something.
\\<org-capture-mode-map>
This will let you select a template from `org-capture-templates', and then
file the newly captured information.  The text is immediately inserted
at the target location, and an indirect buffer is shown where you can
edit it.  Pressing \\[org-capture-finalize] brings you back to the previous state
of Emacs, so that you can continue your work.

When called interactively with a \\[universal-argument] prefix argument GOTO, don't capture
anything, just go to the file/headline where the selected template
stores its notes.  With a double prefix argument \
\\[universal-argument] \\[universal-argument], go to the last note
stored.

When called with a `C-0' (zero) prefix, insert a template at point.

ELisp programs can set KEYS to a string associated with a template
in `org-capture-templates'.  In this case, interactive selection
will be bypassed.

If `org-capture-use-agenda-date' is non-nil, capturing from the
agenda will use the date at point as the default date.  Then, a
`C-1' prefix will tell the capture process to use the HH:MM time
of the day at point (if any) or the current HH:MM time."
  (interactive "P")
  (when (and org-capture-use-agenda-date
	     (eq major-mode 'org-agenda-mode))
    (setq org-overriding-default-time
	  (org-get-cursor-date (equal goto 1))))
  (cond
   ((equal goto '(4)) (org-capture-goto-target))
   ((equal goto '(16)) (org-capture-goto-last-stored))
   (t
    ;; FIXME: Are these needed?
    (let* ((orig-buf (current-buffer))
	   (annotation (if (and (boundp 'org-capture-link-is-already-stored)
				org-capture-link-is-already-stored)
			   (plist-get org-store-link-plist :annotation)
			 (ignore-errors (org-store-link nil))))
	   (entry (or org-capture-entry (org-capture-select-template keys)))
	   initial)
      (setq initial (or org-capture-initial
			(and (org-region-active-p)
			     (buffer-substring (point) (mark)))))
      (when (stringp initial)
	(remove-text-properties 0 (length initial) '(read-only t) initial))
      (when (stringp annotation)
	(remove-text-properties 0 (length annotation)
				'(read-only t) annotation))
      (cond
       ((equal entry "C")
	(customize-variable 'org-capture-templates))
       ((equal entry "q")
	(error "Abort"))
       (t
	(org-capture-set-plist entry)
	(org-capture-put :original-buffer orig-buf
			 :original-file (or (buffer-file-name orig-buf)
					    (and (featurep 'dired)
						 (car (rassq orig-buf
							     dired-buffers))))
			 :original-file-nondirectory
			 (and (buffer-file-name orig-buf)
			      (file-name-nondirectory
			       (buffer-file-name orig-buf)))
			 :annotation annotation
			 :initial initial
			 :return-to-wconf (current-window-configuration)
			 :default-time
			 (or org-overriding-default-time
			     (org-current-time)))
	(org-capture-set-target-location)
	(org-capture-get-template)
	(condition-case error
	    (org-capture-put :template (org-capture-fill-template))
	  ((error quit)
	   (if (get-buffer "*Capture*") (kill-buffer "*Capture*"))
	   (error "Capture abort: %s" error)))

	(setq org-capture-clock-keep (org-capture-get :clock-keep))
	(if (equal goto 0)
	    ;;insert at point
	    (org-capture-insert-template-here)
	  (condition-case error
	      (org-capture-place-template
	       (equal (car (org-capture-get :target)) 'function))
	    ((error quit)
	     (if (and (buffer-base-buffer (current-buffer))
		      (string-match "\\`CAPTURE-" (buffer-name)))
		 (kill-buffer (current-buffer)))
	     (set-window-configuration (org-capture-get :return-to-wconf))
	     (error "Capture template `%s': %s"
		    (org-capture-get :key)
		    (nth 1 error))))
	  (if (and (derived-mode-p 'org-mode)
		   (org-capture-get :clock-in))
	      (condition-case nil
		  (progn
		    (if (org-clock-is-active)
			(org-capture-put :interrupted-clock
					 (copy-marker org-clock-marker)))
		    (org-clock-in)
		    (org-set-local 'org-capture-clock-was-started t))
		(error
		 "Could not start the clock in this capture buffer")))
	  (if (org-capture-get :immediate-finish)
	      (org-capture-finalize)))))))))
