;; Use online archives for downloading emacs packages

(when (>= emacs-major-version 24)
  (require 'package)
  (setq package-enable-at-startup nil)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.milkbox.net/packages/"))
  (package-initialize nil)

  ;; Simplify loading of packages from the network with use-package.el
  ;; (source: https://github.com/jwiegley/use-package)
  (unless (package-installed-p 'use-package)
	(message "%s" "Refreshing package database...")
	(package-refresh-contents)
	(package-install 'use-package))
  (require 'use-package)

  ;; Allow asynchronous processing in emacs
  ;; (source: https://github.com/jwiegley/emacs-async)
  (use-package async :ensure t))

(defmacro nispio/require (name &rest ignored)
  "Simple macro which requires a package by name. "
  `(let* ((name (quote ,name))
	  (package (if (stringp name) (intern name) name)))
     (require package)))

;; If use-package is not loaded, use require instead
(unless (fboundp 'use-package)
  (defalias 'use-package 'nispio/require))

(provide 'nispio/package-config)

(quote "string")
