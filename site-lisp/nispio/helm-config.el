;; Use incremental completion and selection narrowing
;; (source: https://github.com/emacs-helm/helm)
(use-package helm-config :ensure helm)
(require 'helm)

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


(use-package helm-helm-commands :ensure t)))
(define-key helm-command-map (kbd "C-8") 'helm-helm-commands)


;; Helm interface for describe bindings
;; (source: https://github.com/emacs-helm/helm-descbinds)
(use-package helm-descbinds  :ensure t)

;; (let (map)
;;   (if (boundp 'my-magical-map)
;; 	  (setq map 'my-magical-map)
;; 	(setq map global-map))
;;   (define-key map (kbd "C-3") 'eat-dirt)
;;   (define-key map (kbd "C-4") 'buzz-off)
;;   )

(define-key my-map (kbd "C-h b") 'helm-descbinds)



(define-key my-map (kbd "C-8") helm-command-map)

(define-key helm-map (kbd "M-1") 'nispio/helm-full-frame)

(define-key my-map (kbd "C-h a") 'helm-apropos) ;; Replaces apropos-command
(define-key my-map (kbd "C-h p") 'helm-list-elisp-packages)

(define-key my-map (kbd "M-s b") 'nispio/helm-moccur-buffers)
(define-key my-map (kbd "M-s a") 'helm-do-grep)
(define-key my-map (kbd "M-s o") 'helm-occur) ;; Replaces occur
(define-key my-map (kbd "M-s r") 'helm-register)

(provide 'nispio/helm-config)
