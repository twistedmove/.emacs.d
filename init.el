;;;; .emacs

;; Set default load path for lisp files
(add-to-list 'load-path "~/.emacs.d/site-lisp")

;; Basic emacs settings
(global-font-lock-mode 1)	; Enable syntax highlighting
(global-linum-mode)             ; Enable line numbers
(setq inhibit-startup-screen t) ; Disable splash screen
(setq visible-bell t)           ; Disable system beep
(setq transient-mark-mode t)    ; Enable visual feedback on selections
(column-number-mode t)		; Show column number on mode line
(show-paren-mode)		; Show matching parenthesis

;; Disable useless UI features
(scroll-bar-mode 0)		; Disable scroll bars
(tool-bar-mode 0)		; Disable toolbar
(tooltip-mode 0)		; Disable tooltips
(fringe-mode 0)			; Disable fringes

;; Further customization
(load-theme 'manoj-dark)	; Set color theme
(ido-mode t)			; Enable ido mode (interactively do)
(setq linum-format "%d ")       ; Add space after line numbers
(setq scroll-step 1)		; Only scroll by one line at top/bottom
(setq frame-title-format "emacs - %b")

;; Comment / Uncomment Lines
(global-set-key (kbd "C-c c") 'comment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)

;; Add support for isearch functionality with multiple cursors
;; (source: https://github.com/zk-phi/phi-search.git)
(add-to-list 'load-path "~/.emacs.d/site-lisp/phi-search")
(require 'phi-search)

;; Add support for using multiple cursors
;; (source: https://github.com/magnars/multiple-cursors.el.git)
(add-to-list 'load-path "~/.emacs.d/site-lisp/multiple-cursors")
(require 'multiple-cursors)

;; Customize key bindings for multiple cursors mode
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c C->") 'mc/mark-more-like-this-extended)
(global-set-key (kbd "C-c SPC") 'set-rectangular-region-anchor)
(global-set-key (kbd "C-c C-SPC") (lambda () (interactive) (mc/create-fake-cursor-at-point)))
(global-set-key (kbd "<f7>") 'multiple-cursors-mode)

;; Add support for editing matlab files
;; (source: http://matlab-emacs.cvs.sourceforge.net/viewvc/matlab-emacs/matlab-emacs/?view=tar)
(add-to-list 'load-path "~/.emacs.d/site-lisp/matlab-emacs")
(load-library "matlab-load")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "outline" :height 113 :slant normal :weight normal :width normal))))
 '(column-marker-1 ((t (:background "DarkOrange3"))) t)
 '(font-lock-comment-face ((t (:foreground "green1"))))
 '(font-lock-constant-face ((t (:foreground "gray100"))))
 '(font-lock-function-name-face ((t (:foreground "gray100"))))
 '(font-lock-keyword-face ((t (:foreground "DodgerBlue"))))
 '(font-lock-preprocessor-face ((t (:foreground "DodgerBlue"))))
 '(font-lock-string-face ((t (:foreground "red3"))))
 '(font-lock-type-face ((t (:foreground "DodgerBlue"))))
 '(font-lock-variable-name-face ((t (:foreground "gray100"))))
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
 '(show-paren-mode t)
 '(tool-bar-mode nil))
