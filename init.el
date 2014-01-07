;;;; .emacs

;; Set default load path for lisp files
(add-to-list 'load-path "~/.emacs.d/site-lisp")

;; Basic emacs settings
(global-font-lock-mode 1)               ; Enable syntax highlighting
(global-linum-mode)                     ; Enable line numbers
(setq inhibit-startup-screen t)         ; Disable splash screen
(setq visible-bell t)                   ; Disable system beep
(setq transient-mark-mode t)            ; Enable visual feedback on selections
(column-number-mode t)                  ; Show column number on mode line
(show-paren-mode)                       ; Show matching parenthesis
(electric-pair-mode 1)					; Enable automatic bracket closing

;; Disable useless UI features
(when window-system
  (scroll-bar-mode 0)                   ; Disable scroll bars
  (tool-bar-mode 0)                     ; Disable toolbar
  (tooltip-mode 0)                      ; Disable tooltips
  (fringe-mode 0))                      ; Disable fringes

;; Further customization
(load-theme 'manoj-dark)                ; Set color theme
(ido-mode t)                            ; Enable ido mode (interactively do)
(setq linum-format "%d ")               ; Add space after line numbers
(setq scroll-step 1)                    ; Only scroll by one line at top/bottom
(setq-default truncate-lines t)         ; Truncate lines by default
(setq display-time-day-and-date t)      ; Dispaly date along with time in status bar
(display-time)                          ; Display date and time in status bar
(setq require-final-newline t)          ; Always end a file with a newline
(setq frame-title-format "emacs - %b")  ; Set frame title to "emacs - <buffer name>"

;; ;; Manually set time zone to MST/MDT to fix problems with Cygwin/Windows
;; (setenv "TZ" "MST+7MDT,M4.1.0/2,M10.5.0/2")

;; If not in a TTY, Unbind C-m so that we can use it elsewhere
(unless (not window-system)
  (define-key input-decode-map [?\C-m] [C-m])
  ;; In Org Mode, use <C-m> as <M-return>
  (defun my-fake-M-RET ()
    (interactive)
    (let ((command (key-binding (kbd "<M-return>"))))
      (setq last-command-event [M-return])
      (setq this-command command)
      (call-interactively command)))
  (add-hook 'org-mode-hook (lambda () (local-set-key (kbd "<C-m>") 'my-fake-M-RET))))

;; Use unix line endings by default
(setq default-buffer-file-coding-system 'utf-8-unix)

;; Set up indenting in C/C++
(setq c-default-style "linux")
(setq-default c-basic-offset 4)
(setq-default tab-width 4)
(c-set-offset 'inline-open 0)

;; Other keybindings
(global-set-key (kbd "C-\\") 'other-window)
(global-set-key (kbd "C-c c") 'comment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-q") 'kill-buffer-and-window)
(global-set-key (kbd "<f12>") 'compile)
(define-key ctl-x-map (kbd "<f1>") (lambda () (interactive) (message buffer-file-name)))
(define-key ctl-x-map (kbd "<f5>") 'revert-buffer)
(define-key ctl-x-map (kbd "<f6>") 'add-file-local-variable)

;; Shortcut for opening and closing braces in c-mode
(defun nispio/insert-braces ()
  (interactive)
  (execute-kbd-macro '[return 123 tab return return 125 tab 16 tab]))
(defun nispio/insert-braces-hook ()
  (local-set-key (kbd "<C-m>") 'nispio/insert-braces))
(add-hook 'c-mode-common-hook 'nispio/insert-braces-hook)

;; Use dired-x to add the ability to open all marked files at once
(eval-after-load "dired"
  '(progn
	 (load "dired-x")
	 (define-key dired-mode-map "F" 'dired-do-find-marked-files)))

;; Easy buffer swapping
;; (source: http://www.emacswiki.org/emacs/download/buffer-move.el)
(require 'buffer-move)
(global-set-key (kbd "<C-up>")     'buf-move-up)
(global-set-key (kbd "<C-down>")   'buf-move-down)
(global-set-key (kbd "<C-left>")   'buf-move-left)
(global-set-key (kbd "<C-right>")  'buf-move-right)

;; Make sure that the cygwin bash executable can be found (Windows Emacs)
(when (eq system-type 'windows-nt)
  (setq explicit-shell-file-name "C:/cygwin/bin/bash.exe")
  (setq shell-file-name explicit-shell-file-name)
  (add-to-list 'exec-path "C:/cygwin/bin"))

;; Add an easy way to produce dummy text
;; (source: http://www.emacswiki.org/emacs/download/lorem-ipsum.el)
(require 'lorem-ipsum)
(global-set-key (kbd "C-c C-l") 'Lorem-ipsum-insert-paragraphs)

;; Add support for isearch functionality with multiple cursors
;; (source: https://github.com/zk-phi/phi-search.git)
(add-to-list 'load-path "~/.emacs.d/site-lisp/phi-search")
(require 'phi-search)
;; Make phi-search the default instead of isearch (I think I like it better)
(global-set-key (kbd "C-s") 'phi-search)
(global-set-key (kbd "C-r") 'phi-search-backward)

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

;; Keybindings for multiple cursors mode in TTY
(global-set-key (kbd "M-[ 1 ; 6 n") 'mc/mark-next-like-this)
(global-set-key (kbd "M-[ 1 ; 6 l") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c M-[ 1 ; 6 l") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c M-[ 1 ; 6 n") 'mc/mark-more-like-this-extended)

;; Unfortunately, multiple-cursors falls short on rectangular selection
;;   so I use rect-mark.el to fill in the gaps for now
;; (source: http://www.emacswiki.org/emacs/rect-mark.el)
(global-set-key (kbd "C-x r C-SPC") 'rm-set-mark)
(global-set-key (kbd "C-x r C-x") 'rm-exchange-point-and-mark)
(global-set-key (kbd "C-x r C-w") 'rm-kill-region)
(global-set-key (kbd "C-x r M-w") 'rm-kill-ring-save)
(global-set-key (kbd "C-x r C-y") 'yank-rectangle)
(autoload 'rm-set-mark "rect-mark"
  "Set mark for rectangle." t)
(autoload 'rm-exchange-point-and-mark "rect-mark"
  "Exchange point and mark for rectangle." t)
(autoload 'rm-kill-region "rect-mark"
  "Kill a rectangular region and save it in the kill ring." t)
(autoload 'rm-kill-ring-save "rect-mark"
  "Copy a rectangular region to the kill ring." t)

;; Add support for editing matlab files
;; (source: http://matlab-emacs.cvs.sourceforge.net/viewvc/matlab-emacs/matlab-emacs/?view=tar)
(add-to-list 'load-path "~/.emacs.d/site-lisp/matlab-emacs")
(load-library "matlab-load")

;; Enable column markers at column 81 to warn of long lines
;; (source: http://www.emacswiki.org/emacs/download/column-marker.el)
(require 'column-marker)
(defun marker-at-81 () (interactive) (column-marker-1 81))
(add-hook 'matlab-mode-hook 'marker-at-81)
(add-hook 'c-mode-hook 'marker-at-81)
(add-hook 'c++-mode-hook 'marker-at-81)
(setq matlab-comment-column 50)
(setq-default fill-column 81)

;; Add AUCTeX Mode for generating LaTeX documents
;; (source: http://ftp.gnu.org/pub/gnu/auctex/auctex-11.87.tar.gz)
(add-to-list 'load-path "~/.emacs.d/site-lisp/auctex")
(add-to-list 'load-path "~/.emacs.d/site-lisp/auctex/preview")
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(setq
  TeX-auto-save t
  TeX-parse-self t
  TeX-source-correlate-method (quote synctex)
  TeX-source-correlate-mode t
  TeX-source-correlate-start-server t
  reftex-plug-into-AUCTeX t
  TeX-view-program-list (quote (("Sumatra PDF" "/usr/local/bin/sumatra -reuse-instance %o")))
  TeX-view-program-selection (quote ((output-pdf "Sumatra PDF"))))
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

;; Add Helm support for advanced searching
;; (source: https://github.com/emacs-helm/helm.git)
(add-to-list 'load-path "~/.emacs.d/site-lisp/helm")
(require 'helm-config)

;; Make helm search open buffers automatically
;; (source: http://stackoverflow.com/q/14726601)
(eval-after-load "helm-regexp"
  '(helm-attrset 'follow 1 helm-source-moccur))
(defun my-helm-multi-all ()
  "multi-occur in all buffers backed by files."
  (interactive)
  (helm-multi-occur
   (delq nil
         (mapcar (lambda (b)
                   (when (buffer-file-name b) (buffer-name b)))
                 (buffer-list)))))
(global-set-key (kbd "S-<f3>") 'my-helm-multi-all)
(global-set-key (kbd "C-c C-h") 'my-helm-multi-all)

;; Add sublimity mode for mini-map
;; (source: https://github.com/zk-phi/sublimity.git)
(add-to-list 'load-path "~/.emacs.d/site-lisp/sublimity")
(require 'sublimity)
(require 'sublimity-scroll)
(require 'sublimity-map)

;; Add support for Chrome extension "Edit with Emacs"
;; (source: https://github.com/stsquad/emacs_chrome.git)
(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs_chrome/servers")
(when (require 'edit-server nil t)
    (setq edit-server-new-frame nil)
    (add-hook 'edit-server-start-hook 'flyspell-mode)
    (add-hook 'edit-server-start-hook 'visual-line-mode)
    (add-hook 'edit-server-started-hook 'delete-other-windows)
    (add-hook 'edit-server-buffer-closed-hook 'delete-window)
    (edit-server-start))

;; Use DejaVu Sans Mono as default font
;; (source: http://sourceforge.net/projects/dejavu/files/dejavu/2.34/dejavu-fonts-ttf-2.34.tar.bz2)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "outline" :height 113 :slant normal :weight normal :width normal))))
 '(column-marker-1 ((t (:background "DarkOrange3"))))
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
 '(safe-local-variable-values (quote ((visual-line-mode . t) (auto-fill-mode . 0))))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(put 'dired-find-alternate-file 'disabled nil)
