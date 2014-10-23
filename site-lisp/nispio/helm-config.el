;; Use incremental completion and selection narrowing
;; (source: https://github.com/emacs-helm/helm)
(use-package helm :ensure t)
(require 'helm-config)

;; Turn on follow mode when using multi-occur
(require 'helm-regexp)
(eval-after-load "helm-regexp"
  '(setq helm-source-moccur
	 (helm-make-source "Moccur" 'helm-source-multi-occur :follow 1)))

;; (source: http://stackoverflow.com/q/14726601)
(defun nispio/helm-moccur-buffers ()
  "multi-occur in all buffers backed by files."
  (interactive)
  (helm-multi-occur
   (delq nil
	 (mapcar (lambda (b)
		   (when (buffer-file-name b) (buffer-name b)))
		 (buffer-list)))))


;; (source: http://emacs.stackexchange.com/a/650/93)
(defun nispio/helm-full-frame ()
  (interactive)
  (with-selected-window (helm-window)
    (delete-other-windows)))


;; (use-package helm-helm-commands :ensure t)
;; (require 'helm-helm-commands)
;; (define-key helm-command-map (kbd "C-8") 'helm-helm-commands)


;; Helm interface for describe bindings
;; (source: https://github.com/emacs-helm/helm-descbinds)
(use-package helm-descbinds  :ensure t)
(require 'helm-descbinds)

(provide 'nispio/helm-config)
