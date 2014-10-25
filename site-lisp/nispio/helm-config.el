;; Use Helm for incremental completion and selection narrowing
;; (source: https://github.com/emacs-helm/helm)
(use-package helm :ensure t)
(require 'helm-config)
(require 'helm-regexp)

;; Helm interface for describe bindings
;; (source: https://github.com/emacs-helm/helm-descbinds)
(use-package helm-descbinds  :ensure t)

;; Use Helm to choose from a list of Helm commands
;; (source: https://github.com/vapniks/helm-helm-commands)
(use-package helm-helm-commands :ensure t)



(defvar nispio/helm-follow-sources ()
  "List of sources for which helm-follow-mode should be enabled")

;; Use helm-follow-mode for the following sources:
(add-to-list 'nispio/helm-follow-sources 'helm-source-occur)
(add-to-list 'nispio/helm-follow-sources 'helm-source-moccur)

(defun nispio/helm-set-follow ()
  "Enable helm-follow-mode for the sources specified in the list
variable `nispio/helm-follow-sources'. This function is meant to
be run during `helm-initialize' and should be added to the hook
`helm-before-initialize-hook'."
  (mapc (lambda (source)
		  (when (memq source nispio/helm-follow-sources)
			(helm-attrset 'follow 1 (symbol-value source))))
		helm-sources))

;; Add hook to enable helm-follow mode for specified helm 
(add-hook 'helm-before-initialize-hook 'nispio/helm-set-follow)



;; Do helm-multi-occur in all buffers backed by files
;; (source: http://stackoverflow.com/q/14726601)
(defun nispio/helm-moccur-buffers ()
  "helm-multi-occur in all buffers backed by files."
  (interactive)
  (helm-multi-occur
   (delq nil
	 (mapcar (lambda (b)
		   (when (buffer-file-name b) (buffer-name b)))
		 (buffer-list)))))



;; Expand the Helm window to take up the entire frame
;; (source: http://emacs.stackexchange.com/a/650/93)
(defun nispio/helm-full-frame ()
  (interactive)
  (with-selected-window (helm-window)
    (delete-other-windows)))



(provide 'nispio/helm-config)
