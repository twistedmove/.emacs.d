(setq debug-on-error t)

;; Basic emacs settings
(show-paren-mode)                       ; Show matching parenthesis
(setq-default truncate-lines t)         ; Truncate lines by default

;; Essential key bindings
(global-set-key (kbd "C-\\") 'other-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "C-c c") 'comment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)

;; Make sure that we don't unintentionally load packages from ELPA
(require 'package)
(setq package-enable-at-startup nil)

(add-to-list 'load-path "~/.emacs.d/elpa/helm-20140924.1130")
(require 'helm-config)
(require 'helm-regexp)
(eval-after-load "helm-regexp"
  '(setq helm-source-moccur
	 (helm-make-source "Moccur" 'helm-source-multi-occur :follow 1)))


;; (eval-after-load "helm-regexp"
;;   (progn
;;     (setq helm-source-moccur
;; 	  (helm-make-source "Moccur"
;; 			    'helm-source-multi-occur
;; 			    :follow 1))

;;   (defun nispio/helm-multi-occur-buffers ()
;;     "multi-occur in all buffers backed by files."
;;     (interactive)
;;     (helm-multi-occur
;;      (delq nil
;; 	   (mapcar (lambda (b)
;; 		     (when (buffer-file-name b) (buffer-name b)))
;; 		   (buffer-list)))))
;;   (global-set-key (kbd "M-s b") 'nispio/helm-multi-occur-buffers) t))
