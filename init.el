;;;; .emacs

(switch-to-buffer "*Messages*")

;; Set load paths for lisp files
(add-to-list 'load-path "~/.emacs.d/site-lisp")

;; Load my own minor mode for personal keybindings
(require 'nispio/my-mode)
(enable-my-global-mode)
(global-set-key (kbd "C-M-&") 'enable-my-global-mode)

  (setq-default truncate-lines t)	 ; Truncate lines by default
  (setq inhibit-startup-screen t)	 ; Disable splash screen
  (setq visible-bell t)				 ; Disable system beep
  (setq transient-mark-mode t)		 ; Enable visual feedback on selections
  (setq x-stretch-cursor t)			 ; Cursor as wide as the glyph under it
  (setq display-time-day-and-date t) ; Dispaly date along with time in status bar
  (setq scroll-step 1)				 ; Only scroll by one line at top/bottom
  (setq require-final-newline t)	 ; Always end a file with a newline
  (setq frame-title-format "emacs - %b") ; Set frame title to "emacs - <buffer name>"

;; Load init files as appropriate, turning errors into messages
(with-demoted-errors "INIT ERROR: %s"
  ;; Simple minor modes
  (show-paren-mode 1)				 ; Show matching parenthesis
  (global-font-lock-mode 1)			 ; Enable syntax highlighting
  (column-number-mode t)			 ; Show column number on mode line
  (show-paren-mode)					 ; Show matching parenthesis
  (display-time)					 ; Display date and time in status bar
  (ido-mode 1)

  (when (>= emacs-major-version 24)
	(electric-pair-mode 1))			 ; Enable automatic bracket closing

  (when (>= emacs-major-version 24)
	(require 'nispio/package-config))

  (define-key my-map (kbd "M-s n") 'find-name-dired)

  ;; Use a more powerful alternative to ido-mode's flex matching.
  ;; (source: https://github.com/lewang/flx.git)
  (use-package flx-ido :ensure t)
  

  (setq load-path nil)
  (require 'nispio/dummy-mode)


  ;; Manage and navigate projects easily in Emacs
  ;; (source: https://github.com/bbatsov/projectile.git)
  (use-package projectile
	:ensure t
	:init
	(projectile-global-mode)
	(setq projectile-enable-caching t)

	;; Emacs frontend to GNU Global source code tagging system.
	;; (source: https://github.com/leoliu/ggtags)
	(use-package ggtags :ensure t)

	;; Use helm for projectile
	(use-package helm-projectile
	  :ensure t
	  :init
	  (helm-projectile-on))
	
	(define-key my-map (kbd "C-7") projectile-command-map)
	(define-key projectile-command-map (kbd "\\") 'projectile-find-other-file))


  ;; Display line numbers in all programming buffers
  (use-package linum
	:ensure
	:init
	(progn
	  (add-hook 'prog-mode-hook 'linum-mode)
	  (setq linum-format "%3d")))

  ;; If not in a TTY, Unbind C-m so that we can use it elsewhere
  (unless (not window-system)
	(define-key input-decode-map [?\C-m] [C-m])
	;; In Org Mode, use <C-m> as <M-return>
	(defun nispio/fake-M-RET ()
	  (interactive)
	  (let ((command (key-binding (kbd "<M-return>"))))
		(setq last-command-event [M-return])
		(setq this-command command)
		(call-interactively command)))
	(add-hook 'org-mode-hook (lambda () (local-set-key (kbd "<C-m>") 'nispio/fake-M-RET))))

  ;; Modify the behavior of `org-table-align`
  (load-file "~/.emacs.d/nispio/org-table-align.el")

  ;; Use unix line endings by default
  (setq default-buffer-file-coding-system 'utf-8-unix)

  ;; Extend dired functionality
  (use-package dired+
	:ensure t
	:config
	(progn
	  (require 'dired-x)
	  ;; Command to open all marked files at once
	  (define-key dired-mode-map (kbd "F") 'dired-do-find-marked-files)
	  (define-key dired-mode-map (kbd "/") 'phi-search)
	  ;; When opening a directory in dired, reuse the current buffer
	  (diredp-toggle-find-file-reuse-dir 1)
	  (customize-set-variable 'diredp-hide-details-initially-flag nil)))


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
  (use-package buffer-move
	:ensure t
	:config
	(define-key my-map (kbd "C-c <C-up>") 'buf-move-up)
	(define-key my-map (kbd "C-c <C-down>") 'buf-move-down)
	(define-key my-map (kbd "C-c <C-left>") 'buf-move-left)
	(define-key my-map (kbd "C-c <C-right>") 'buf-move-right))

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
  (use-package lorem-ipsum
	:ensure t
	:init
	(define-key my-map (kbd "C-c C-l")  'Lorem-ipsum-insert-paragraphs))

  ;; Add support for isearch functionality with multiple cursors
  ;; (source: https://github.com/zk-phi/phi-search)
  (use-package phi-search
	:ensure t
	:config
	(customize-set-value 'phi-search-case-sensitive 'guess)
	(define-key my-map (kbd "C-s") 'phi-search)
	(define-key my-map (kbd "C-r") 'phi-search-backward))


  ;; Add support for editing with multiple cursors
  ;; (source: https://github.com/magnars/multiple-cursors.el)
  (use-package multiple-cursors
	:ensure t
	:init
	(progn
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
	  (when (package-installed-p 'phi-search)
		(use-package phi-search-mc
		  :ensure t
		  :init (phi-search-mc/setup-keys)))))

  (define-key my-map (kbd "C-c c") 'comment-region)
  (define-key my-map (kbd "C-c u") 'uncomment-region)

  (define-key my-map (kbd "C-x <f5>") 'revert-buffer)
  (define-key my-map (kbd "C-x <f6>") 'add-file-local-variable)

  ;; Unfortunately, multiple-cursors falls short on rectangular selection
  ;;   so I use rect-mark.el to fill in the gaps for now
  ;; (source: http://www.emacswiki.org/emacs/rect-mark.el)
  (use-package phi-rectangle
	:ensure t
	:init 
	(define-key my-map (kbd "C-c C-SPC") 'phi-rectangle-set-mark-command)
	(define-key my-map (kbd "C-w") 'phi-rectangle-kill-region)
	(define-key my-map (kbd "M-w") 'phi-rectangle-kill-ring-save)
	(define-key my-map (kbd "C-y") 'phi-rectangle-yank))

  ;; Add support for editing matlab files
  ;; (source: http://matlab-emacs.cvs.sourceforge.net/viewvc/matlab-emacs/matlab-emacs/?view=tar)
  (use-package "matlab-load"
	:ensure matlab-mode
	:init
	(matlab-cedet-setup)
	(setq matlab-comment-column 50)
	(require 'matlab-debug))

  ;; Enable column markers at column 81 to warn of long lines
  ;; (source: http://www.emacswiki.org/emacs/download/column-marker.el)
  (use-package column-marker
	:ensure t
	:init (progn
			(defun nispio/column-marker-at-81 ()
			  (interactive)
			  (column-marker-1 81))
			(add-hook 'prog-mode-hook 'nispio/column-marker-at-81)
			(setq-default fill-column 81)))

  ;; ;; Set up auto-complete
  ;; ;; (source: https://github.com/auto-complete/auto-complete)
  ;; (add-to-list 'load-path "~/.emacs.d/site-lisp/auto-complete")
  ;; (add-to-list 'load-path "~/.emacs.d/site-lisp/auto-complete/lib/popup")
  ;; (add-to-list 'load-path "~/.emacs.d/site-lisp/auto-complete/lib/fuzzy")
  ;; (add-to-list 'load-path "~/.emacs.d/site-lisp/auto-complete/lib/erc")
  (use-package auto-complete-config
	:ensure auto-complete
	:init
	(ac-config-default)
	;; start after 3 characters were typed
	(setq ac-auto-start 3)
	;; show menu immediately...
	(setq ac-auto-show-menu t)
	;; explicit call to auto-complete
	(define-key ac-mode-map (kbd "C-.") 'auto-complete)
	;; Allow auto-complete with matlab-mode
	(setq ac-modes (cons 'matlab-mode ac-modes))
	)

  (use-package tex-site
	:ensure auctex
	:init
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
	(add-hook 'LaTeX-mode-hook 'turn-on-reftex))

  ;; Add support for Chrome extension "Edit with Emacs"
  ;; (source: https://github.com/stsquad/emacs_chrome.git)
  (use-package edit-server
	:ensure t
	:init
	(setq edit-server-new-frame nil)
	(add-hook 'edit-server-start-hook 'flyspell-mode)
	(add-hook 'edit-server-start-hook 'visual-line-mode)
	(add-hook 'edit-server-started-hook 'delete-other-windows)
	(add-hook 'edit-server-buffer-closed-hook 'delete-window)
	(edit-server-start))

  ;; SrSpeedbar allows a speedbar that is "docked" in the current frame
  (use-package sr-speedbar
	:ensure t
	:config
	(define-key my-map (kbd "C-c M-SPC") 'sr-speedbar-toggle)
	(define-key my-map (kbd "C-c C-g w") 'sr-speedbar-select-window))


  ;; Display ^L as a horizontal line
  (use-package page-break-lines
	:ensure t
	:init
	(require 'page-break-lines)
	(global-page-break-lines-mode)
	(diminish 'page-break-lines-mode ""))

  ;; Use powerline for a nifty mode line
  (use-package powerline
	:ensure t
	:init
	(require 'powerline)
	(powerline-default-theme)
	(setq powerline-default-separator 'wave))

  (load-file "~/.emacs.d/nispio/init-devel.el")

  (if (display-graphic-p)
	  (load-file "~/.emacs.d/init-gui.el"))

  (load-file "~/.emacs.d/nispio/bindings.el")
  (load-file "~/.emacs.d/nispio/xmidas.el")
  (load-file "~/.emacs.d/nispio/dsv-to-orgtbl.el")
  (load-file "~/.emacs.d/nispio/org-table-header.el")
  (load-file "~/.emacs.d/nispio/org-table-align.el")

  )

;; Settings modified via the Customize interface get their own file
(if (display-graphic-p)
    (setq custom-file "~/.emacs.d/settings.el")
  (setq custom-file "~/.emacs.d/settings-tty.el"))
(load custom-file)
