;; Configure UI features
(scroll-bar-mode 0)			; Disable scroll bars
(tooltip-mode 1)			; Disable tooltips
(fringe-mode '(nil . 0))		; Left fringes only
(tool-bar-mode 0)			; Disable toolbar

(when (>= emacs-major-version 24)
  ;; Use powerline for a nifty mode line
  (use-package powerline :ensure t)
  (require 'powerline)
  (powerline-default-theme)
  (setq powerline-default-separator 'wave))

(load-file "~/.emacs.d/site-lisp/nispio/themes/nispio-dark.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Buffer-menu-name-width 50)
 '(column-number-mode t)
 '(dired-omit-files "^\\.?#")
 '(display-time-mode t)
 '(font-lock-maximum-decoration (quote ((t . t) (dired-mode . 2))))
 '(font-use-system-font t)
 '(helm-buffer-max-length 30)
 '(helm-truncate-lines t)
 '(ibuffer-elide-long-columns t)
 '(ibuffer-formats
   (quote
	((mark modified read-only " "
		   (name 35 35 :left :elide)
		   " "
		   (size 9 -1 :right)
		   " "
		   (mode 16 16 :left :elide)
		   " " filename-and-process)
	 (mark " "
		   (name 16 -1)
		   " " filename))))
 '(ibuffer-saved-filter-groups
   (quote
	(("default"
	  ("File Buffers"
	   (filename . ".+"))
	  ("Dired Buffers"
	   (used-mode . dired-mode))
	  ("Special Buffers"
	   (name . "\\b\\*.*\\*\\b"))))))
 '(ibuffer-saved-filters
   (quote
	(("gnus"
	  ((or
		(mode . message-mode)
		(mode . mail-mode)
		(mode . gnus-group-mode)
		(mode . gnus-summary-mode)
		(mode . gnus-article-mode))))
	 ("programming"
	  ((or
		(mode . emacs-lisp-mode)
		(mode . cperl-mode)
		(mode . c-mode)
		(mode . java-mode)
		(mode . idl-mode)
		(mode . lisp-mode)))))))
 '(ido-default-buffer-method (quote selected-window))
 '(ido-default-file-method (quote selected-window))
 '(ido-max-window-height 2)
 '(minimap-dedicated-window nil)
 '(minimap-display-semantic-overlays nil)
 '(minimap-hide-fringes t)
 '(minimap-recenter-type (quote relative))
 '(minimap-resizes-buffer t)
 '(mlint-programs (quote ("/usr/local/MATLAB/R2014a/bin/glnxa64/mlint")))
 '(projectile-mode-line
   (quote
	(:eval
	 (format " Prj[%s]"
			 (projectile-project-name)))))
 '(safe-local-variable-values (quote ((visual-line-mode . t) (auto-fill-mode . 0))))
 '(save-interprogram-paste-before-kill t)
 '(show-paren-mode t)
 '(sr-speedbar-skip-other-window-p t)
 '(yank-pop-change-selection t))
(put 'dired-find-alternate-file 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
