;;;###autoload
(defun nispio/find-dired (dir args)
  "Run `find' and go into Dired mode on a buffer of the output.
The command run (after changing into DIR) is essentially

    find . \\( ARGS \\) -ls

except that the car of the variable `find-ls-option' specifies what to
use in place of \"-ls\" as the final argument."
  (let ((dired-buffers dired-buffers))
    ;; Expand DIR ("" means default-directory), and make sure it has a
    ;; trailing slash.
    (setq dir (file-name-as-directory (expand-file-name dir)))
    ;; Check that it's really a directory.
    (or (file-directory-p dir)
	(error "find-dired needs a directory: %s" dir))
    (switch-to-buffer (get-buffer-create "*Find*"))

    ;; See if there's still a `find' running, and offer to kill
    ;; it first, if it is.
    (let ((find (get-buffer-process (current-buffer))))
      (when find
	(if (or (not (eq (process-status find) 'run))
		(yes-or-no-p "A `find' process is running; kill it? "))
	    (condition-case nil
		(progn
		  (interrupt-process find)
		  (sit-for 1)
		  (delete-process find))
	      (error nil))
	  (error "Cannot have two processes in `%s' at once" (buffer-name)))))

    (widen)
    (kill-all-local-variables)
    (setq buffer-read-only nil)
    (erase-buffer)
    (setq default-directory dir
	  args (concat find-program " . "
		       (if (string= args "")
			   ""
			 (concat
			  (shell-quote-argument "(")
			  " " args " "
			  (shell-quote-argument ")")
			  " "))
		       (if (string-match "\\`\\(.*\\) {} \\(\\\\;\\|+\\)\\'"
					 (car find-ls-option))
			   (format "%s %s %s"
				   (match-string 1 (car find-ls-option))
				   (shell-quote-argument "{}")
				   find-exec-terminator)
			 (car find-ls-option))))
    ;; Start the find process.
    (shell-command (concat args "&") (current-buffer))
    ;; The next statement will bomb in classic dired (no optional arg allowed)
    (dired-mode dir (cdr find-ls-option))
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map (current-local-map))
      (define-key map "\C-c\C-k" 'kill-find)
      (use-local-map map))
    (make-local-variable 'dired-sort-inhibit)
    (setq dired-sort-inhibit t)
    (set (make-local-variable 'revert-buffer-function)
	 `(lambda (ignore-auto noconfirm)
	    (find-dired ,dir ,find-args)))
    ;; Set subdir-alist so that Tree Dired will work:
    (if (fboundp 'dired-simple-subdir-alist)
	;; will work even with nested dired format (dired-nstd.el,v 1.15
	;; and later)
	(dired-simple-subdir-alist)
      ;; else we have an ancient tree dired (or classic dired, where
      ;; this does no harm)
      (set (make-local-variable 'dired-subdir-alist)
	   (list (cons default-directory (point-min-marker)))))
    (set (make-local-variable 'dired-subdir-switches) find-ls-subdir-switches)
    (setq buffer-read-only nil)
    ;; Subdir headlerline must come first because the first marker in
    ;; subdir-alist points there.
    (insert "  " dir ":\n")
    ;; Make second line a ``find'' line in analogy to the ``total'' or
    ;; ``wildcard'' line.
    (insert "  " args "\n")
    (setq buffer-read-only t)
    (let ((proc (get-buffer-process (current-buffer))))
      (set-process-filter proc (function find-dired-filter))
      (set-process-sentinel proc (function find-dired-sentinel))
      ;; Initialize the process marker; it is used by the filter.
      (move-marker (process-mark proc) 1 (current-buffer)))
    (setq mode-line-process '(":%s"))))

;;;###autoload
(defun nispio/find-iname-dired (pattern)
  "Search marked directories recursively for files matching the globbing pattern
PATTERN, and run dired on those files.
PATTERN is a shell wildcard (not an Emacs regexp) and need not be quoted."
  (interactive "sFind-name (filename wildcard): ")
  (nispio/find-dired dired-directory
	      (concat (mapconcat 'identity (dired-get-marked-files) " ")
		      "-iname " (shell-quote-argument pattern)))

;;;###autoload
(defun nispio/find-regexp-dired (pattern)
  "Search marked directories recursively for files matching the regexp PATTERN,
and run dired on those files.
PATTERN is an Emacs regexp and need not be quoted."
  (interactive "sFind-name (filename regexp): ")
  (nispio/find-dired dired-directory
	      (concat (mapconcat 'identity (dired-get-marked-files) " ")
		      "-regexp " (shell-quote-argument pattern)))

