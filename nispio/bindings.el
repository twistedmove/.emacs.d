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

(bind-key "C-h k" 'nispio/locate-key-binding)
(bind-key* "C-h C-M-k" 'nispio/unbind-local-key)
