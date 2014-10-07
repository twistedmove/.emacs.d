;; Use DejaVu Sans Mono as default font
;; (source: http://sourceforge.net/projects/dejavu/files/dejavu/2.34/dejavu-fonts-ttf-2.34.tar.bz2)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :foreground "WhiteSmoke" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 102 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(column-marker-1 ((t (:background "DarkOrange3"))))
 '(cursor ((t (:background "magenta"))))
 '(diredp-dir-heading ((t (:foreground "orange red"))))
 '(diredp-dir-priv ((t (:background "black"))))
 '(diredp-exec-priv ((t nil)))
 '(diredp-executable-tag ((t nil)))
 '(diredp-ignored-file-name ((t (:foreground "#444444"))))
 '(diredp-link-priv ((t nil)))
 '(diredp-no-priv ((t nil)))
 '(diredp-other-priv ((t nil)))
 '(diredp-rare-priv ((t nil)))
 '(diredp-read-priv ((t nil)))
 '(diredp-write-priv ((t (:foreground "RosyBrown2"))))
 '(font-lock-comment-face ((t (:foreground "green1"))))
 '(font-lock-constant-face ((t (:foreground "gray100"))))
 '(font-lock-function-name-face ((t (:foreground "gray100"))))
 '(font-lock-keyword-face ((t (:foreground "DodgerBlue"))))
 '(font-lock-preprocessor-face ((t (:foreground "DodgerBlue"))))
 '(font-lock-string-face ((t (:foreground "red3"))))
 '(font-lock-type-face ((t (:foreground "DodgerBlue"))))
 '(font-lock-variable-name-face ((t (:foreground "gray100"))))
 '(fringe ((t (:background "black" :foreground "Wheat"))))
 '(matlab-operator-face ((t (:foreground "gray100"))) t)
 '(minibuffer-prompt ((t (:foreground "cyan1"))))
 '(mode-line ((t (:inverse-video t :box nil))))
 '(mode-line-highlight ((t (:box nil))))
 '(mode-line-inactive ((t (:box nil))))
 '(org-table ((t (:foreground "DodgerBlue"))) t))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Buffer-menu-name-width 50)
 '(column-number-mode t)
 '(font-lock-maximum-decoration (quote ((t . t) (dired-mode . 2))))
 '(ibuffer-elide-long-columns t)
 '(ibuffer-formats (quote ((mark modified read-only " " (name 35 35 :left :elide) " " (size 9 -1 :right) " " (mode 16 16 :left :elide) " " filename-and-process) (mark " " (name 16 -1) " " filename))))
 '(ibuffer-saved-filter-groups (quote (("default" ("File Buffers" (filename . ".+")) ("Dired Buffers" (used-mode . dired-mode)) ("Special Buffers" (name . "\\b\\*.*\\*\\b"))))))
 '(ibuffer-saved-filters (quote (("gnus" ((or (mode . message-mode) (mode . mail-mode) (mode . gnus-group-mode) (mode . gnus-summary-mode) (mode . gnus-article-mode)))) ("programming" ((or (mode . emacs-lisp-mode) (mode . cperl-mode) (mode . c-mode) (mode . java-mode) (mode . idl-mode) (mode . lisp-mode)))))))
 '(ido-max-window-height 2)
 '(linum-format "%3d ")
 '(minimap-dedicated-window nil)
 '(minimap-display-semantic-overlays nil)
 '(minimap-hide-fringes t)
 '(minimap-recenter-type (quote relative))
 '(minimap-resizes-buffer t)
 '(mlint-programs (quote ("/usr/local/MATLAB/R2014a/bin/glnxa64/mlint")))
 '(safe-local-variable-values (quote ((visual-line-mode . t) (auto-fill-mode . 0))))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(put 'dired-find-alternate-file 'disabled nil)
