;;;; .emacs

(message "Welcome to Emacs!\nThis session brought to you by:\n%s"
		 (mapconcat 'identity command-line-args " "))

(message "Loading init file...")
(switch-to-buffer "*Messages*")

;; My "must-have" key bindings get set before anything can go wrong.
(global-set-key (kbd "<C-tab>") 'next-multiframe-window)
(global-set-key (kbd "<C-iso-lefttab>") 'previous-multiframe-window)
(global-set-key (kbd "M-1") 'delete-other-windows)

;; Set load paths for lisp files
(setq site-lisp "~/.emacs.d/site-lisp")
(add-to-list 'load-path site-lisp)

(require 'nispio/misc-utils)
(setq load-path
	  (append
	   ;; Add all folders from site-lisp (except explicitly rejected dirs).
	   ;; The "nispio" dir is not included because its packages are provided as
	   ;; (provide 'nispio/package-name)
	   (nispio/directory-subdirs site-lisp '(".git" "nispio"))
	   ;; Put the current load-path at the end
	   load-path))

;; Load my own minor mode for personal keybindings
(require 'nispio/my-mode)
(enable-my-global-mode)
(global-set-key (kbd "C-M-&") 'enable-my-global-mode)

(define-key my-map (kbd "C-H-\\") 'nispio/switch-to-scratch-and-back)
(define-key my-map (kbd "M-s N") 'nispio/dired-find-exts)

(require 'nispio/key-utils)
(nispio/unbind-digit-arguments)
(define-key my-map (kbd "C-h C-k") 'nispio/insert-key-description)
(define-key my-map (kbd "C-h k") 'nispio/locate-key-binding)
(global-set-key (kbd "C-h C-M-k") 'nispio/unbind-local-key)

;; This is a hack because my M-s keybinding disappear in some modes
(define-key my-map (kbd "M-s") (key-binding (kbd "M-s")))

(require 'nispio/rect-utils)
(define-key my-map (kbd "C-x r Y") 'nispio/yank-rectangle-from-kill-ring)
(define-key my-map (kbd "C-x r D") 'delete-whitespace-rectangle)

(define-key my-map (kbd "C-c c") 'comment-region)
(define-key my-map (kbd "C-c u") 'uncomment-region)
(define-key my-map (kbd "C-x <f5>") 'revert-buffer)
(define-key my-map (kbd "C-x <f6>") 'add-file-local-variable)

(require 'nispio/org-config)
(define-key my-map (kbd "C-c a") 'org-agenda)


;; Basic editor configuration
(setq-default truncate-lines t)        ; Truncate lines by default
(setq inhibit-startup-screen t)        ; Disable splash screen
(setq visible-bell t)                  ; Disable system beep
(setq transient-mark-mode t)           ; Enable visual feedback on selections
(setq x-stretch-cursor t)              ; Cursor as wide as the glyph under it
(setq scroll-step 1)                   ; Only scroll by one line at top/bottom
(setq require-final-newline t)         ; Always end a file with a newline
(setq frame-title-format "emacs - %b") ; Set frame title to "emacs - <buffer name>"

;; Set tab width to 4 and put tab stops every 4 characters
(setq-default tab-width 4)
(setq tab-stop-list (number-sequence 4 100 4))



;; Load init files as appropriate, turning errors into messages
(with-demoted-errors "INIT ERROR: %s"
  ;; Simple minor modes
  (show-paren-mode 1)                ; Show matching parenthesis
  (global-font-lock-mode 1)          ; Enable syntax highlighting
  (column-number-mode t)             ; Show column number on mode line
  (setq display-time-day-and-date t) ; Dispaly date along with time in status bar
  (display-time)                     ; Display date and time in status bar
  (ido-mode 1)
  
  (require 'nispio/package-config)

  ;; Display line numbers in all programming buffers
  (use-package linum :ensure t)
  ;(global-linum-mode -1)
  (add-hook 'prog-mode-hook 'linum-mode)
  (setq linum-format "%3d")

  ;; In Org Mode, use <C-m> as <M-return>
  (defun nispio/fake-M-RET ()
	(interactive)
	(let ((command (key-binding (kbd "<M-return>"))))
	  (setq last-command-event [M-return])
	  (setq this-command command)
	  (call-interactively command)))
  (add-hook 'org-mode-hook (lambda () (local-set-key (kbd "<C-m>") 'nispio/fake-M-RET)))

  ;; Use unix line endings by default
  (setq default-buffer-file-coding-system 'utf-8-unix)

  (require 'dired-x)
  ;; Start search in dired buffer with "/"
  (define-key dired-mode-map (kbd "/") 'dired-isearch-filenames)
  (define-key dired-mode-map "F" 'nispio/find-marked-files)

  ;; ;; ;; Extend dired functionality
  ;; ;; (use-package dired+ :ensure t)

  ;; ;; When opening a directory in dired, reuse the current buffer
  ;; (diredp-toggle-find-file-reuse-dir 1)
  ;; (customize-set-variable 'diredp-hide-details-initially-flag nil)

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
  (define-key my-map (kbd "C-c C-l")  'Lorem-ipsum-insert-paragraphs)

  ;; Add support for isearch functionality with multiple cursors
  ;; (source: https://github.com/zk-phi/phi-search)
  (use-package phi-search :ensure t)
  (customize-set-value 'phi-search-case-sensitive 'guess)
  (define-key my-map (kbd "H-C-s") 'phi-search)
  (define-key my-map (kbd "H-C-r") 'phi-search-backward)

  ;; Complete phi-search with the match selected
  (defun phi-search-complete-with-selection ()
  (interactive)
  (let ((query (buffer-string)))
    (phi-search-complete)
    (mc/execute-command-for-all-cursors
     (lambda ()
       (interactive)
       (when (looking-back query)
         (push-mark (match-beginning 0) t t)
         (goto-char (match-end 0))
         (activate-mark))))))
  (define-key phi-search-default-map (kbd "<S-return>") 'phi-search-complete-with-selection)

  ;; Add support for editing with multiple cursors
  ;; (source: https://github.com/magnars/multiple-cursors.el)
  (use-package multiple-cursors :ensure t)
  (defun nispio/fake-cursor-at-point ()
    (interactive)
    (mc/create-fake-cursor-at-point))

  (define-key my-map (kbd "C->") 'mc/mark-next-like-this)
  (define-key my-map (kbd "C-<") 'mc/mark-previous-like-this)
  (define-key my-map (kbd "C-c C-<") 'mc/mark-all-like-this)
  (define-key my-map (kbd "C-c C->") 'mc/mark-more-like-this-extended)
  (define-key my-map (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (define-key my-map (kbd "C-S-c C-<") 'mc/mark-all-in-region)
  (define-key my-map (kbd "H-C-SPC") 'nispio/fake-cursor-at-point)
  (define-key my-map (kbd "<H-return>") 'multiple-cursors-mode)

  ;; TODO: find a better way to set these bindings in special cases
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
  (global-set-key (kbd "C-c C->") 'mc/mark-more-like-this-extended)
  (global-set-key (kbd "H-C-SPC") 'nispio/fake-cursor-at-point)
  (global-set-key (kbd "<H-return>") 'multiple-cursors-mode)
  
  ;; Add extended interoperability between phi-search and multiple cursors
  ;; (source: https://github.com/knu/phi-search-mc.el)
  (eval-after-load "phi-search"
    (progn
	  (use-package phi-search-mc :ensure t)
      (phi-search-mc/setup-keys)
      (phi-search-from-isearch-mc/setup-keys)
      nil))

  ;; Use phi-rectangle for rectangular selections
  (use-package phi-rectangle :ensure t)
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
  (add-hook 'matlab-mode-hook 'linum-mode)
  ;; Use CEDET tools for matlab-mode
  (when (>= emacs-major-version 24)
    (matlab-cedet-setup)
  	)

  ;; Enable column markers at column 81 to warn of long lines
  ;; (source: http://www.emacswiki.org/emacs/download/column-marker.el)
  ;; (use-package column-marker :ensure t)
  (require 'column-marker)
  (defun nispio/column-marker-at-81 ()
    (interactive)
    (column-marker-1 81))
  (add-hook 'prog-mode-hook 'nispio/column-marker-at-81)
  (setq-default fill-column 80)


  ;; (use-package tex-site :ensure auctex)
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
  ;; (use-package sr-speedbar :ensure t)
  (require 'sr-speedbar)
  (define-key my-map (kbd "C-c M-SPC") 'sr-speedbar-toggle)
  ;(define-key nispio/gdb-window-map (kbd "w") 'sr-speedbar-select-window)

  ;; Display ^L as a horizontal line
  ;; (use-package page-break-lines :ensure t)
  (require 'page-break-lines)
  (global-page-break-lines-mode)
  (diminish 'page-break-lines-mode "")
  
  (require 'nispio/dev-utils)
  (define-key nispio/gdb-window-map (kbd "w") 'sr-speedbar-select-window)
  (define-key my-map (kbd "H-g") nispio/gdb-window-map)

  (require 'nispio/xmidas)

  ) ;; end with-demoted-errors



;; Load packages that are only compatible with Emacs 24.3+
(when (and (= emacs-major-version 24) (>= emacs-minor-version 3))
  (with-demoted-errors "INIT ERROR: %s"
    (electric-pair-mode 1) ; Enable automatic bracket closing

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
    (define-key my-map (kbd "M-s O") 'helm-multi-occur)
    (define-key my-map (kbd "M-s r") 'helm-register)

	(use-package helm-swoop :ensure t)
	(define-key my-map (kbd "M-s i") 'helm-swoop)
	(define-key my-map (kbd "M-s I") 'helm-multi-swoop)
	(define-key my-map (kbd "M-s B") 'helm-multi-swoop-all)

	(let ((map isearch-mode-map))
	  (define-key map [remap isearch-occur] 'helm-occur-from-isearch)
	  (define-key map (kbd "H-M-s o") 'helm-occur-from-isearch)
	  (define-key map (kbd "H-M-s O") 'helm-multi-occur-from-isearch)
	  (define-key map (kbd "H-M-s i") 'helm-swoop-from-isearch)
	  (define-key map (kbd "H-M-s B") 'helm-multi-swoop-all-from-isearch))


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

	(defvar project-root-regexps ()
	  "List of regexps to match against when projectile is searching
for project root directories.")

	;; File containing local project roots on this machine
	(let ((file "~/.emacs.d/local-projects.el"))
	  (when (file-exists-p file)
		(load-file file)))
  
	;; Add the ability to use projects that are not 
	(eval-after-load 'projectile
	  (progn 
		;; (source: https://github.com/bbatsov/projectile/issues/364#issuecomment-61296248)
		(defun projectile-root-child-of (dir &optional list)
		  (projectile-locate-dominating-file
		   dir
		   (lambda (dir)
			 (--first
			  (if (and
				   (s-equals? (file-remote-p it) (file-remote-p dir))
				   (string-match-p (expand-file-name it) (expand-file-name dir)))
				  dir)
			  (or list project-root-regexps (list))))))
		(nconc projectile-project-root-files-functions '(projectile-root-child-of))
		nil))



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
    (setq ac-auto-start 3)              ; start after 3 characters were typed
    (setq ac-auto-show-menu t)          ; show menu immediately...
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

	) ;; end with-demote-errors
  ) ;; end emacs 24.3+ customizations



;; isearch automatically wraps upon failure
;; (source: http://stackoverflow.com/q/285660/1590790)
(defadvice isearch-search (after isearch-no-fail activate)
  (unless isearch-success
    (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)
    (isearch-repeat (if isearch-forward 'forward))
    (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)))

;; Activate semantic for all programming modes
(add-hook 'prog-mode-hook 'semantic-mode)

;; Set the default faces for highlighting with hi-lock
(setq hi-lock-face-defaults (list "hi-yellow" "hi-pink" "hi-green" "hi-blue"))

;; Function to turn a delimiter-separated value file into an org table
(autoload 'nispio/edit-dsv-as-orgtbl "nispio/org-table-utils")
(global-set-key (kbd "C-c |") 'nispio/edit-dsv-as-orgtbl)

;; Add some personal org-mode tweaks centered around editing org tables
(autoload 'nispio/org-mode-setup "nispio/org-table-utils")
(add-hook 'org-mode-hook 'nispio/org-mode-setup)

;; Make hide-show mode available, turn it on it a buffer with C-c @
(autoload 'hs-minor-mode "hideshow")
(global-set-key (kbd "C-c @") 'hs-minor-mode)
(defvar nispio/hs-mode-map 
  (let ((map (make-sparse-keymap)))
	(define-key map (kbd "C-2") 'hs-toggle-hiding)
	(define-key map (kbd "C-c") 'hs-toggle-hiding)
	(define-key map (kbd "C-h") 'hs-hide-block)
	(define-key map (kbd "C-l") 'hs-hide-level)
	(define-key map (kbd "C-s") 'hs-show-block)
	(define-key map (kbd "C-M-h") 'hs-hide-all)
	(define-key map (kbd "C-M-s") 'hs-show-all)
	map ))
(define-key my-map (kbd "C-2") nispio/hs-mode-map)

;; Add [] to the list of collapsible entries in js mode (for JSON files)
(eval-after-load "hideshow"
  (progn
	(add-to-list 'hs-special-modes-alist
				 '(js-mode "\\({\\|\\[\\)" "\\(}\\|\\]\\)" "/[*/]" nil))
	nil))

;; Use hideshow mode in javascript mode by default
(add-hook 'js-mode-hook 'hs-minor-mode)

;; Key bindings for calc
(require 'calc)
(define-key calc-mode-map (kbd "<backtab>") 'calc-roll-up)
(define-key my-map (kbd "H-*") 'nispio/calc-grab-number)

;; Key bindings for re-builder
(require 're-builder)
(define-key reb-mode-map (kbd "C-c %") 'nispio/reb-query-replace)

;; Enable disabled commands
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(setq no-server-start-p (member "--no-server" command-line-args))
(setq command-line-args (delete "--no-server" command-line-args))

(when (display-graphic-p)
  (unless no-server-start-p
	;; Start (or restart) the server
	(require 'server)
	(server-force-delete)
	(server-start)))




;; Settings modified via the Customize interface get their own file
(if (display-graphic-p)
    (setq custom-file "~/.emacs.d/settings.el")
  (setq custom-file "~/.emacs.d/settings-tty.el"))
(load custom-file)
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
