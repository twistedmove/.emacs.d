;; Use online archives for downloading emacs packages

(when (>= emacs-major-version 24)
  (require 'package)
  (setq package-enable-at-startup nil)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.milkbox.net/packages/"))
  ;;(setq package-archives '(("local-elpa" . "/user_data4/emacs/local-elpa/")))
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
  "Simple macro which requires a package by name."
  `(let* ((name (quote ,name))
	  (package (if (stringp name) (intern name) name)))
     (require package)))

;; If use-package is not loaded, use require instead
(unless (fboundp 'use-package)
  (defalias 'use-package 'nispio/require))



(defun nispio/create-custom-archive (archive-dir)
  (let* ((elpa-contents-file (format "%s/archive-contents.elpa" archive-dir))
		 (elpa-contents (nispio/read-archive-contents elpa-contents-file))
		 (melpa-contents-file (format "%s/archive-contents.melpa" archive-dir))
		 (melpa-contents (nispio/read-archive-contents melpa-contents-file))
		 (whitelist-file (format "%s/archive-whitelist" archive-dir))
		 (whitelist (nispio/read-archive-contents whitelist-file))
		 (dest-file (format "%s/archive-contents" archive-dir))
		 (archive (list)))
	(dolist (package (append elpa-contents melpa-contents))
	  (if (memq (car package) whitelist)
		  (setq archive (cons package archive))))
	(when archive
	  (nispio/write-archive-to-file archive dest-file))
	archive))

(defun nispio/read-archive-contents (filename)
  (if (file-exists-p filename)
	(with-temp-buffer
	  (insert-file-contents-literally filename)
	  (let ((contents (read (current-buffer))))
		(cdr contents)))
	(error "Cannot find file %s" filename)))

(defun nispio/write-archive-to-file (archive file)
   (with-temp-buffer
     (insert (prin1-to-string (cons '1 archive)))
     (when (file-writable-p file)
       (write-region (point-min)
                     (point-max)
                     file))))



(provide 'nispio/package-config)
