;; Use DejaVu Sans Mono as default font
;; (source: http://sourceforge.net/projects/dejavu/files/dejavu/2.34/dejavu-fonts-ttf-2.34.tar.bz2)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "outline" :height 113 :slant normal :weight normal :width normal))))
 '(column-marker-1 ((t (:background "DarkOrange3"))))
 '(diredp-dir-heading ((t (:foreground "orange red"))))
 '(diredp-dir-priv ((t (:background "black"))))
 '(diredp-exec-priv ((t nil)))
 '(diredp-executable-tag ((t nil)))
 '(diredp-ignored-file-name ((t (:foreground "#333333"))))
 '(diredp-link-priv ((t nil)))
 '(diredp-no-priv ((t nil)))
 '(diredp-other-priv ((t nil)))
 '(diredp-rare-priv ((t nil)))
 '(diredp-read-priv ((t nil)))
 '(diredp-write-priv ((t nil)))
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
 '(mode-line ((t (:box nil))))
 '(mode-line-highlight ((t (:box nil))))
 '(mode-line-inactive ((t (:box nil))))
 '(org-table ((t (:foreground "DodgerBlue"))) t))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(font-lock-maximum-decoration (quote ((t . t) (dired-mode . 2))))
 '(ido-max-window-height 2)
 '(safe-local-variable-values (quote ((visual-line-mode . t) (auto-fill-mode . 0))))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(put 'dired-find-alternate-file 'disabled nil)
