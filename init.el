;;;; .emacs

(message "Loading init file...")
(switch-to-buffer "*Messages*")

(global-set-key (kbd "C-\\") 'other-window)
(global-set-key (kbd "M-1") 'delete-other-windows)

;; Set load paths for lisp files
(add-to-list 'load-path "~/.emacs.d/site-lisp")

;; Load my own minor mode for personal keybindings
(require 'nispio/my-mode)
(enable-my-global-mode)
(global-set-key (kbd "C-M-&") 'enable-my-global-mode)

(require 'nispio/misc-utils) ;; for nispio/trim-string
(require 'nispio/key-utils)
(define-key my-map (kbd "C-h C-k") 'nispio/insert-key-description)
(define-key my-map (kbd "C-h k") 'nispio/locate-key-binding)
(global-set-key (kbd "C-h C-M-k") 'nispio/unbind-local-key)

(require 'nispio/org-table-utils)
(global-set-key (kbd "C-c |") 'nispio/edit-dsv-as-orgtbl)

(setq-default truncate-lines t)	 ; Truncate lines by default
(setq inhibit-startup-screen t)	 ; Disable splash screen
(setq visible-bell t)				 ; Disable system beep
(setq transient-mark-mode t)		 ; Enable visual feedback on selections
(setq x-stretch-cursor t)			 ; Cursor as wide as the glyph under it
(setq scroll-step 1)				 ; Only scroll by one line at top/bottom
(setq require-final-newline t)	 ; Always end a file with a newline
(setq frame-title-format "emacs - %b") ; Set frame title to "emacs - <buffer name>"

(add-to-list 'load-path "~/.emacs.d/site-lisp/async")
(add-to-list 'load-path "~/.emacs.d/site-lisp/auctex")
(add-to-list 'load-path "~/.emacs.d/site-lisp/auto-complete")
(add-to-list 'load-path "~/.emacs.d/site-lisp/bind-key")
(add-to-list 'load-path "~/.emacs.d/site-lisp/buffer-move")
(add-to-list 'load-path "~/.emacs.d/site-lisp/column-marker")
(add-to-list 'load-path "~/.emacs.d/site-lisp/dash")
(add-to-list 'load-path "~/.emacs.d/site-lisp/diminish")
(add-to-list 'load-path "~/.emacs.d/site-lisp/dired+")
(add-to-list 'load-path "~/.emacs.d/site-lisp/edit-server")
(add-to-list 'load-path "~/.emacs.d/site-lisp/epl")
(add-to-list 'load-path "~/.emacs.d/site-lisp/flx")
(add-to-list 'load-path "~/.emacs.d/site-lisp/flx-ido")
(add-to-list 'load-path "~/.emacs.d/site-lisp/ggtags")
(add-to-list 'load-path "~/.emacs.d/site-lisp/gnupg")
(add-to-list 'load-path "~/.emacs.d/site-lisp/helm")
(add-to-list 'load-path "~/.emacs.d/site-lisp/helm-descbinds")
(add-to-list 'load-path "~/.emacs.d/site-lisp/helm-projectile")
(add-to-list 'load-path "~/.emacs.d/site-lisp/lorem-ipsum")
(add-to-list 'load-path "~/.emacs.d/site-lisp/matlab-mode")
(add-to-list 'load-path "~/.emacs.d/site-lisp/multiple-cursors")
(add-to-list 'load-path "~/.emacs.d/site-lisp/page-break-lines")
(add-to-list 'load-path "~/.emacs.d/site-lisp/phi-rectangle")
(add-to-list 'load-path "~/.emacs.d/site-lisp/phi-search")
(add-to-list 'load-path "~/.emacs.d/site-lisp/phi-search-mc")
(add-to-list 'load-path "~/.emacs.d/site-lisp/pkg-info")
(add-to-list 'load-path "~/.emacs.d/site-lisp/popup")
(add-to-list 'load-path "~/.emacs.d/site-lisp/powerline")
(add-to-list 'load-path "~/.emacs.d/site-lisp/projectile")
(add-to-list 'load-path "~/.emacs.d/site-lisp/s")
(add-to-list 'load-path "~/.emacs.d/site-lisp/sr-speedbar")
(add-to-list 'load-path "~/.emacs.d/site-lisp/use-package")

;; Load init files as appropriate, turning errors into messages
(with-demoted-errors "INIT ERROR: %s"
  ;; Simple minor modes
  (show-paren-mode 1)				 ; Show matching parenthesis
  (global-font-lock-mode 1)			 ; Enable syntax highlighting
  (column-number-mode t)			 ; Show column number on mode line
  (setq display-time-day-and-date t) ; Dispaly date along with time in status bar
  (display-time)					 ; Display date and time in status bar
  (ido-mode 1)

  (when (and (>= emacs-major-version 24) (>= emacs-minor-version 4))
    (electric-pair-mode 1) ; Enable automatic bracket closing

    (require 'nispio/package-config)
    (require 'diminish)
    
    (require 'nispio/helm-config)
    (define-key helm-map (kbd "M-1") 'nispio/helm-full-frame)
    (define-key my-map (kbd "M-s n") 'find-name-dired)
    (define-key my-map (kbd "C-h b") 'helm-descbinds)
    (define-key my-map (kbd "C-8") helm-command-map)
    (define-key my-map (kbd "C-h a") 'helm-apropos) ;; Replaces apropos-command
    (define-key my-map (kbd "C-h p") 'helm-list-elisp-packages)
    (define-key my-map (kbd "M-s b") 'nispio/helm-moccur-buffers)
    (define-key my-map (kbd "M-s a") 'helm-do-grep)
    (define-key my-map (kbd "M-s o") 'helm-occur) ;; Replaces occur
    (define-key my-map (kbd "M-s r") 'helm-register)

    ;; Use a more powerful alternative to ido-mode's flex matching.
    ;; SOURCE: https://github.com/lewang/flx.git
    (use-package flx-ido :ensure t)
    (require 'flx-ido)

    ;; Manage and navigate projects easily in Emacs
    ;; SOURCE: https://github.com/bbatsov/projectile.git
    (use-package projectile :ensure t)
    (require 'projectile)
    (projectile-global-mode)
    (setq projectile-enable-caching t)

    ;; Use helm for projectile
    (use-package helm-projectile :ensure t)
    (eval-after-load "helm-config"
      (progn
		(require 'helm-projectile)
		(helm-projectile-on)
		(define-key my-map (kbd "C-7") projectile-command-map)
		(define-key projectile-command-map (kbd "\\") 'projectile-find-other-file)
		nil))
	
    ;; Emacs frontend to GNU Global source code tagging system.
    ;; SOURCE: https://github.com/leoliu/ggtags
    (use-package ggtags :ensure t)
    (require 'ggtags)

    ;; Set up auto-complete
    ;; (source: https://github.com/auto-complete/auto-complete)
    (use-package auto-complete :ensure t)
    (require 'auto-complete-config)
    (ac-config-default)
    (setq ac-auto-start 3)				; start after 3 characters were typed
    (setq ac-auto-show-menu t)			; show menu immediately...
    (setq ac-modes (cons 'matlab-mode ac-modes))  ; Allow auto-complete with matlab-mode
    (define-key ac-mode-map (kbd "C-.") 'auto-complete)

    ;; Add support for Chrome extension "Edit with Emacs"
    ;; (source: https://github.com/stsquad/emacs_chrome.git)
    (use-package edit-server :ensure t)
    (require 'edit-server)
    (setq edit-server-new-frame nil)
    (add-hook 'edit-server-start-hook 'flyspell-mode)
    (add-hook 'edit-server-start-hook 'visual-line-mode)
    (add-hook 'edit-server-started-hook 'delete-other-windows)
    (add-hook 'edit-server-buffer-closed-hook 'delete-window)
    (edit-server-start)

    ;; Use powerline for a nifty mode line
    (use-package powerline :ensure t)
    (require 'powerline)
    (powerline-default-theme)
    (setq powerline-default-separator 'wave)

  ) ;; end emacs 24+ customizations
  
  ;; Display line numbers in all programming buffers
  (use-package linum :ensure t)
  (require 'linum)
  (global-linum-mode 1)
  (add-hook 'prog-mode-hook 'linum-mode)
  (setq linum-format "%3d")

  ;; (unless tty-keys
  ;;   ;; In Org Mode, use <C-m> as <M-return>
  ;;   (defun nispio/fake-M-RET ()
  ;;     (interactive)
  ;;     (let ((command (key-binding (kbd "<M-return>"))))
  ;; 	(setq last-command-event [M-return])
  ;; 	(setq this-command command)
  ;; 	(call-interactively command)))
  ;;   (add-hook 'org-mode-hook (lambda () (local-set-key (kbd "<C-m>") 'nispio/fake-M-RET))))

  ;; Use unix line endings by default
  (setq default-buffer-file-coding-system 'utf-8-unix)

  ;; Extend dired functionality
  (use-package dired+ :ensure t)
  (require 'dired-x)
  (require 'dired+)
  ;; Command to open all marked files at once
  (define-key dired-mode-map (kbd "F") 'dired-do-find-marked-files)
  (define-key dired-mode-map (kbd "/") 'phi-search)
  ;; When opening a directory in dired, reuse the current buffer
  (diredp-toggle-find-file-reuse-dir 1)
  (customize-set-variable 'diredp-hide-details-initially-flag nil)


  ;; Make ibuffer auto-update after changes
  ;; (source: http://emacs.stackexchange.com/a/2179/93)
  (defun nispio/ibuffer-stale-p (&optional noconfirm)
	(frame-or-buffer-changed-p 'ibuffer-auto-buffers-changed))
  (defun nispio/ibuffer-auto-revert-setup ()
	(set (make-local-variable 'buffer-stale-function)
		 'nispio/ibuffer-stale-p)
	(setq-local auto-revert-verbose nil)
	(auto-revert-mode 1))
  (add-hook 'ibuffer-mode-hook 'nispio/ibuffer-auto-revert-setup)

  ;; Easily re-arrange buffers within the frame
  ;; (source: http://www.emacswiki.org/emacs/download/buffer-move.el)
  (use-package buffer-move :ensure t)
  (require 'buffer-move)
  (define-key my-map (kbd "C-c <C-up>") 'buf-move-up)
  (define-key my-map (kbd "C-c <C-down>") 'buf-move-down)
  (define-key my-map (kbd "C-c <C-left>") 'buf-move-left)
  (define-key my-map (kbd "C-c <C-right>") 'buf-move-right)

  ;; Keybindings to change the window size
  (define-key my-map (kbd "C-S-<left>") 'shrink-window-horizontally)
  (define-key my-map (kbd "C-S-<right>") 'enlarge-window-horizontally)
  (define-key my-map (kbd "C-S-<up>") 'shrink-window)
  (define-key my-map (kbd "C-S-<down>") 'enlarge-window)
  (define-key my-map (kbd "M-o 6 d") 'shrink-window-horizontally)
  (define-key my-map (kbd "M-o 6 c") 'enlarge-window-horizontally)
  (define-key my-map (kbd "M-o 6 a") 'shrink-window)
  (define-key my-map (kbd "M-o 6 b") 'enlarge-window)

  ;; Make sure that the cygwin bash executable can be found (Windows Emacs)
  (when (eq system-type 'windows-nt)
	(setq explicit-shell-file-name "C:/cygwin/bin/bash.exe")
	(setq shell-file-name explicit-shell-file-name)
	(add-to-list 'exec-path "C:/cygwin/bin"))

  ;; Add an easy way to produce dummy text
  ;; (source: http://www.emacswiki.org/emacs/download/lorem-ipsum.el)
  (use-package lorem-ipsum :ensure t)
  (require 'lorem-ipsum)
  (define-key my-map (kbd "C-c C-l")  'Lorem-ipsum-insert-paragraphs)

  ;; Add support for isearch functionality with multiple cursors
  ;; (source: https://github.com/zk-phi/phi-search)
  (use-package phi-search :ensure t)
  (require 'phi-search)
  (customize-set-value 'phi-search-case-sensitive 'guess)
  (define-key my-map (kbd "C-s") 'phi-search)
  (define-key my-map (kbd "C-r") 'phi-search-backward)

  ;; Add support for editing with multiple cursors
  ;; (source: https://github.com/magnars/multiple-cursors.el)
  (use-package multiple-cursors	:ensure t)
  (require 'multiple-cursors)
  (defun nispio/fake-cursor-at-point ()
	(interactive)
	(mc/create-fake-cursor-at-point))

  (define-key my-map (kbd "C-c C-SPC") 'nispio/fake-cursor-at-point)
  (define-key my-map (kbd "C->") 'mc/mark-next-like-this)
  (define-key my-map (kbd "C-<") 'mc/mark-previous-like-this)
  (define-key my-map (kbd "C-c C-<") 'mc/mark-all-like-this)
  (define-key my-map (kbd "C-c C->") 'mc/mark-more-like-this-extended)
  (define-key my-map (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (define-key my-map (kbd "C-S-c C-<") 'mc/mark-all-in-region)
  (define-key my-map (kbd "<f7>") 'multiple-cursors-mode)

  ;; Add extended interoperability between phi-search and multiple cursors
  ;; (source: https://github.com/knu/phi-search-mc.el)
  (use-package phi-search-mc :ensure t)
  (eval-after-load "phi-search"
    (progn
      (require 'phi-search-mc)
      (phi-search-mc/setup-keys)
      nil))

  (define-key my-map (kbd "C-c c") 'comment-region)
  (define-key my-map (kbd "C-c u") 'uncomment-region)

  (define-key my-map (kbd "C-x <f5>") 'revert-buffer)
  (define-key my-map (kbd "C-x <f6>") 'add-file-local-variable)

  ;; Use phi-rectangle for rectangular selections
  ;; (source: http://www.emacswiki.org/emacs/rect-mark.el)
  (use-package phi-rectangle :ensure t)
  (require 'phi-rectangle)
  (define-key my-map (kbd "C-c C-SPC") 'phi-rectangle-set-mark-command)
  (define-key my-map (kbd "C-w") 'phi-rectangle-kill-region)
  (define-key my-map (kbd "M-w") 'phi-rectangle-kill-ring-save)
  (define-key my-map (kbd "C-y") 'phi-rectangle-yank)

  ;; Add support for editing matlab files
  ;; (source: http://matlab-emacs.cvs.sourceforge.net/viewvc/matlab-emacs/matlab-emacs/?view=tar)
  (use-package "matlab-load" :ensure matlab-mode)
  (require 'matlab-load)
  (require 'nispio/matlab-debug)
  (setq matlab-comment-column 50)
  ;; Use CEDET tools for matlab-mode
  (when (>= emacs-major-version 24)
    (matlab-cedet-setup)
  )

  ;; Enable column markers at column 81 to warn of long lines
  ;; (source: http://www.emacswiki.org/emacs/download/column-marker.el)
  (use-package column-marker :ensure t)
  (require 'column-marker)
  (defun nispio/column-marker-at-81 ()
	(interactive)
	(column-marker-1 81))
  (add-hook 'prog-mode-hook 'nispio/column-marker-at-81)
  (setq-default fill-column 80)


  (use-package tex-site	:ensure auctex)
  (require 'tex-site)
  (setq
   TeX-auto-save t
   TeX-parse-self t
   TeX-source-correlate-method (quote synctex)
   TeX-source-correlate-mode t
   TeX-source-correlate-start-server t
   reftex-plug-into-AUCTeX t)
  ;; (setq
  ;;  TeX-view-program-list (quote (("Sumatra PDF" "/usr/local/bin/sumatra -reuse-instance %o")))
  ;;  TeX-view-program-selection (quote ((output-pdf "Sumatra PDF"))))
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)

  ;; SrSpeedbar allows a speedbar that is "docked" in the current frame
  (use-package sr-speedbar :ensure t)
  (require 'sr-speedbar)
  (define-key my-map (kbd "C-c M-SPC") 'sr-speedbar-toggle)
  (define-key my-map (kbd "C-c C-g w") 'sr-speedbar-select-window)

  ;; Display ^L as a horizontal line
  (use-package page-break-lines	:ensure t)
  (require 'page-break-lines)
  (global-page-break-lines-mode)
  (diminish 'page-break-lines-mode "")
  
  ;; TODO: require devel-utils
  (load-file "~/.emacs.d/site-lisp/nispio/init-devel.el")

  (if (display-graphic-p)
	  (load-file "~/.emacs.d/init-gui.el"))

  (require 'nispio/xmidas)

  ) ;; end with-demoted-errors

;; Settings modified via the Customize interface get their own file
(if (display-graphic-p)
    (setq custom-file "~/.emacs.d/settings.el")
  (setq custom-file "~/.emacs.d/settings-tty.el"))
(load custom-file)
