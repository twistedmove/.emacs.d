;; Configure UI features
(scroll-bar-mode 0)			; Disable scroll bars
(tooltip-mode 1)			; Enable tooltips
(fringe-mode '(nil . 0))	; Left fringes only
(tool-bar-mode 0)			; Disable toolbar

;; (when (>= emacs-major-version 24)
;;   ;; Use powerline for a nifty mode line
;;   (use-package powerline :ensure t)
;;   (require 'powerline)
;;   (powerline-default-theme)
;;   (setq powerline-default-separator 'wave))

(load-file "~/.emacs.d/site-lisp/nispio/themes/nispio-dark.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Buffer-menu-name-width 50)
 '(column-number-mode t)
 '(dired-omit-files "^\\.?#")
 '(doxymacs-doxygen-style "C++")
 '(font-lock-maximum-decoration (quote ((t . t) (dired-mode . 2))))
 '(font-use-system-font t)
 '(helm-buffer-max-length 30)
 '(helm-match-plugin-mode t nil (helm-match-plugin))
 '(helm-split-window-in-side-p t)
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
 '(mc/edit-lines-empty-lines (quote ignore))
 '(minimap-dedicated-window nil)
 '(minimap-display-semantic-overlays nil)
 '(minimap-hide-fringes t)
 '(minimap-recenter-type (quote relative))
 '(minimap-resizes-buffer t)
 '(mlint-programs (quote ("/usr/local/MATLAB/R2014a/bin/glnxa64/mlint")))
 '(org-agenda-files "~/.org/agendas.ini")
 '(org-directory "~/.org")
 '(page-break-lines-modes
   (quote
	(emacs-lisp-mode lisp-mode scheme-mode compilation-mode outline-mode help-mode c-mode c++-mode python-mode)))
 '(projectile-mode-line (quote (:eval (format " [%s]" (projectile-project-name)))))
 '(projectile-require-project-root nil)
 '(python-fill-docstring-style (quote django))
 '(safe-local-variable-values
   (quote
	((org-enforce-todo-dependencies . t)
	 (flymake-master-file-name . "../test/test_pipe.cc")
	 (py-indent-offset . 4)
	 (visual-line-mode . t)
	 (auto-fill-mode . 0))))
 '(save-interprogram-paste-before-kill t)
 '(semantic-default-submodes
   (quote
	(global-semantic-highlight-func-mode global-semantic-stickyfunc-mode global-semanticdb-minor-mode global-semantic-idle-summary-mode global-semantic-idle-local-symbol-highlight-mode)))
 '(show-paren-mode t)
 '(sr-speedbar-skip-other-window-p nil)
 '(yank-pop-change-selection t))
(put 'dired-find-alternate-file 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "black" :foreground "WhiteSmoke"))))
 '(flymake-warnline ((t (:underline (:color "#555500" :style wave)))))
 '(linemark-funny-face ((t (:background "gray14" :foreground "light yellow"))))
 '(linemark-go-face ((t (:background "gray14" :foreground "honeydew1")))))
