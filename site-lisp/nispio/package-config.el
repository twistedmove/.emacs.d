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



(defun nispio/create-custom-archive (whitelist-file &optional repository-list)
  (let* ((dir (file-name-directory whitelist-file))
		 (dest-file (concat dir "archive-contents"))
		 (whitelist (nispio/read-contents whitelist-file))
		 my-archive name added full-name readme-name)
	(setq repository-list (or repository-list
							  '("http://melpa.org/packages/"
								"http://elpa.gnu.org/packages/")))
	(dolist (url repository-list)
	  (dolist (package (cdr (nispio/get-archive-contents url)))
		(let* ((name (car package))
			   (full-name (nispio/package-full-name package))
			   (readme-name (concat (symbol-name name) "-readme.txt")))
		(when (and (memq name whitelist) (not (memq name added)))
		  (setq my-archive (cons package my-archive))
		  (setq added (cons name added))
		  (message "Downloading %s" full-name)
		  (url-copy-file (concat url full-name) (concat dir full-name) t)
		  (url-copy-file (concat url readme-name) (concat dir readme-name) t)))))
	(nispio/write-archive-to-file my-archive dest-file)))

(defun nispio/package-full-name (package)
  (let* ((name (symbol-name (car package)))
		 (version (package--ac-desc-version (cdr package)))
		 (version-string (package-version-join version))
		 (kind (package--ac-desc-kind (cdr package)))
		 (suffix (if (eq kind 'tar) "tar" "el")))
	(concat name "-" version-string "." suffix)))

(defun nispio/get-archive-contents (repository-url)
  (let* ((file-url (concat repository-url "archive-contents"))
		 (file (url-file-local-copy file-url)))
	(nispio/read-contents file)))

(defun nispio/read-contents (filename)
  (if (file-exists-p filename)
	(with-temp-buffer
	  (insert-file-contents-literally filename)
	  (let ((contents (read (current-buffer))))
		contents))
	(error "Cannot find file %s" filename)))

(defun nispio/write-archive-to-file (archive file)
  (when archive
	(with-temp-file file
		(insert (prin1-to-string (cons '1 archive)))
	  file)))




(provide 'nispio/package-config)
