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

(defun nispio/show-prefix-arg (&optional arg)
  (interactive "P")
  (if (/= 0 (safe-length arg))
      (message "Prefix: %s (%d)" arg (safe-length arg))
    (message "Argument: %s (%d)" arg (safe-length arg))))


