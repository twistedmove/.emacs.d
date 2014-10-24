;; Unbind a local key binding
(defun nispio/unbind-local-key (key)
  "Remove the local binding for a given key sequence"
  (interactive "kPress key: ")
  (let ((name (key-description key))
    (binding (local-key-binding key)))
    (if (not binding)
    (message "Key is not bound locally: %s" name)
      (local-set-key key nil)
      (message "Unbinding key: %s (was '%s)" name binding))))

;; Look for special keybindings associated with overlays or text properties
;; (source: http://emacs.stackexchange.com/a/654/93)
(defun nispio/key-binding-at-point (key)
  "Find key bindings associated with text properties or overlays at point"
  (mapcar (lambda (keymap) (lookup-key keymap key))
          (cl-remove-if-not
           #'keymapp
           (append
            (mapcar (lambda (overlay)
                      (overlay-get overlay 'keymap))
                    (overlays-at (point)))
            (get-text-property (point) 'keymap)
            (get-text-property (point) 'local-map)))))

;; Show a list of all key bindings for a given key sequence
;; (derived from: http://emacs.stackexchange.com/a/654/93)
(defun nispio/locate-key-binding (key)
  "Determine in which keymap KEY is defined."
  (interactive "kPress key: ")
  (if (key-binding key t)
	  (let* ((desc (key-description key))
			 (function (key-binding key t))
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
			(princ (format "\n%s\n\n%s" usage doc))
			))
		function)
	(message "%s is undefined" (key-description key))))

(defun nispio/insert-key-description (key &optional arg)
"Capture a keybinding directly from the keyboard and insert its string
representation at point. With optional ARG, display the key description in the
minibuffer instead of inserting it at point."
  (interactive "k\nP")
  (let ((desc (key-description key)))
	(if arg (message desc) (insert desc))))

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

(defun nispio/unbind-digit-arguments ()
  "Unbind modified digit keys from the global map"
  (let ((prefix-list '("C-M-" "M-" "C-"))
		(digit-list (number-sequence 0 9))
		digit-string key)
	(dolist (digit digit-list)
	  (setq digit-string (format "%d" digit))
	  (mapc
	   (lambda (prefix-string)
		 (setq key (kbd (concat prefix-string digit-string)))
		 (when (eq (global-key-binding key) 'digit-argument)
		   (global-unset-key key)))
	   prefix-list))))

(provide 'nispio/key-utils)
