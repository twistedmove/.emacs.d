;;;; .emacs

;; Set default load path for lisp files
(add-to-list 'load-path "~/.emacs.d/site-lisp")

;; Basic emacs settings
(global-font-lock-mode 1)               ; Enable syntax highlighting
(setq inhibit-startup-screen t)         ; Disable splash screen
(setq visible-bell t)                   ; Disable system beep
(setq transient-mark-mode t)            ; Enable visual feedback on selections
(column-number-mode t)                  ; Show column number on mode line
(show-paren-mode)                       ; Show matching parenthesis
(electric-pair-mode 1)					; Enable automatic bracket closing
(setq x-stretch-cursor t)				; Cursor as wide as the glyph under it

;; Disable useless UI features
(when window-system
  (scroll-bar-mode 0)                   ; Disable scroll bars
  (tool-bar-mode 0)                     ; Disable toolbar
  (tooltip-mode 0))						; Disable tooltips

;; Further customization
(load-theme 'manoj-dark)                ; Set color theme
(setq scroll-step 1)                    ; Only scroll by one line at top/bottom
(setq-default truncate-lines t)         ; Truncate lines by default
(setq display-time-day-and-date t)      ; Dispaly date along with time in status bar
(display-time)                          ; Display date and time in status bar
(setq require-final-newline t)          ; Always end a file with a newline
(setq frame-title-format "emacs - %b")  ; Set frame title to "emacs - <buffer name>"
(fringe-mode '(nil . 0))				; Left fringes only



;; Use online archives for downloading emacs packages
(require 'package)
(add-to-list
 'package-archives
 '("melpa" . "http://melpa.milkbox.net/packages/"))
(setq package-enable-at-startup nil)
(package-initialize)

;; Simplify loading of packages from the network with use-package.el
;; (source: https://github.com/jwiegley/use-package)
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;; Allow asynchronous processing in emacs
;; (source: https://github.com/jwiegley/emacs-async)
(use-package async :ensure t)

;; Use "ido" completion wherever possible
;; (source: https://github.com/DarwinAwardWinner/ido-ubiquitous)
(use-package ido-ubiquitous
  :ensure t
  :init
  (progn
	(ido-mode 1)			; Enable ido mode (interactively do)
	(ido-ubiquitous-mode 1)))	; Enable ido mode almost everywhere
  
(bind-key "M-s n" 'find-name-dired)
(bind-key* "C-h B" 'describe-personal-keybindings)

;; Use incremental completion and selection narrowing
;; (source: https://github.com/emacs-helm/helm)
(use-package helm-config
  :ensure helm
  :init
  (progn

;; *helm-source-bookmarks
;; *helm-source-buffers-list
;; *helm-source-file-name-history
;; *helm-source-files-in-all-dired
;; *helm-source-files-in-current-dir
;; *helm-source-find-files
;; *helm-source-global-mark-ring
;; *helm-source-grep
;; *helm-source-kill-ring
;; *helm-source-mark-ring
;; *helm-source-moccur
;; *helm-source-name
;; *helm-source-occur
;; *helm-source-recentf
;; *helm-source-regexp
;; *helm-source-register

	;; Helm interface for describe bindings
	;; (source: https://github.com/emacs-helm/helm-descbinds)
    (use-package helm-descbinds
	  :ensure t
      :bind ("C-h b" . helm-descbinds))

	(use-package helm-regexp
	  :init	(helm-attrset 'follow 1 helm-source-moccur))

	;; (source: http://stackoverflow.com/q/14726601)
	(defun nispio/helm-multi-occur-buffers ()
	  "multi-occur in all buffers backed by files."
	  (interactive)
	  (helm-multi-occur
	   (delq nil
			 (mapcar (lambda (b)
					   (when (buffer-file-name b) (buffer-name b)))
					 (buffer-list)))))
	
    (bind-key "C-h a" 'helm-apropos)
    (bind-key "C-c M-x" 'helm-M-x)

	(bind-key "M-s b" 'nispio/helm-multi-occur-buffers)
    (bind-key "M-s a" 'helm-do-grep)
    (bind-key "M-s o" 'helm-occur)
	(bind-key "M-s r" 'helm-register)
	

))


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

;; Use unix line endings by default
(setq default-buffer-file-coding-system 'utf-8-unix)

;; Easier navigation between windows/frames
(defun nispio/other-window (&optional arg)
  "With prefix argument, select the next frame. Otherwise, select the next window"
  (interactive "P")
  (if arg (other-frame 1) (other-window 1)))
(bind-key* "C-\\" 'nispio/other-window)

(defun nispio/buffer-file-name ()
  "Display the name of the file backing the current buffer"
  (interactive)
  (message (or buffer-file-name "no file"))
  buffer-file-name)
(define-key ctl-x-map (kbd "<f1>") 'nispio/buffer-file-name)

;; Other keybindings
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-0") 'kill-buffer-and-window)

;; Extend dired functionality
(use-package dired+
  :ensure t
  :config
  (progn
    (use-package dired-x)
    ;; Open all marked files at once
    (bind-keys :map dired-mode-map
	       ("F" . dired-do-find-marked-files))))


;; Easy buffer swapping
;; (source: http://www.emacswiki.org/emacs/download/buffer-move.el)
(use-package buffer-move
  :ensure t
  :bind
  (("<C-up>" . buf-move-up)
   ("<C-down>" . buf-move-down)
   ("<C-left>" . buf-move-left)
   ("<C-right>" . buf-move-right)))

;; Keybindings to change the window size
(bind-keys
 ("C-S-<left>" . shrink-window-horizontally)
 ("C-S-<right>" . enlarge-window-horizontally)
 ("C-S-<up>" . shrink-window)
 ("C-S-<down>" . enlarge-window)
 ;; Repeat the same bindings for TTY mode
 ("M-o 6 d" . shrink-window-horizontally)
 ("M-o 6 c" . enlarge-window-horizontally)
 ("M-o 6 a" . shrink-window)
 ("M-o 6 b" . enlarge-window))

;; Custom function to toggle fullscreen by maximizing or restoring the current frame.
(defvar nispio/fullscreen-p t "Check if fullscreen is on or off")
(defun nispio/restore-frame ()
  (if (fboundp 'w32-send-sys-command) (w32-send-sys-command 61728)
	(progn (set-frame-parameter nil 'width 82)
		   (set-frame-parameter nil 'fullscreen 'fullheight))))
(defun nispio/maximize-frame ()
  (if (fboundp 'w32-send-sys-command) (w32-send-sys-command 61488)
	(set-frame-parameter nil 'fullscreen 'fullboth)))
(defun nispio/toggle-fullscreen ()
  "Toggle \"fullscreen\" by maximizing or restoring the current frame."
  (interactive)
  (setq nispio/fullscreen-p (not nispio/fullscreen-p))
  (if nispio/fullscreen-p (nispio/restore-frame) (nispio/maximize-frame)))

(bind-key "<f12>" 'nispio/toggle-fullscreen)
(bind-key "<S-f12>" 'delete-frame)

;; Make sure that the cygwin bash executable can be found (Windows Emacs)
(when (eq system-type 'windows-nt)
  (setq explicit-shell-file-name "C:/cygwin/bin/bash.exe")
  (setq shell-file-name explicit-shell-file-name)
  (add-to-list 'exec-path "C:/cygwin/bin"))

;; Add an easy way to produce dummy text
;; (source: http://www.emacswiki.org/emacs/download/lorem-ipsum.el)
(use-package lorem-ipsum
  :ensure t
  :bind ("C-c C-l" . Lorem-ipsum-insert-paragraphs))

;; Add support for isearch functionality with multiple cursors
;; (source: https://github.com/zk-phi/phi-search)
(use-package phi-search
  :ensure t
  :bind 
  (("C-s" . phi-search)
   ("C-r" . phi-search-backward))
  :config
  (customize-set-value 'phi-search-case-sensitive 'guess))

;; Add support for editing with multiple cursors
;; (source: https://github.com/magnars/multiple-cursors.el)
(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)
	 ("C-c C-<" . mc/mark-all-like-this)
	 ("C-c C->" . mc/mark-more-like-this-extended)
	 ("C-S-c C-S-c" . mc/edit-lines)
	 ("C-S-c C-<" . mc/mark-all-in-region)
	 ("<f7>" . multiple-cursors-mode)
	 ;; Keybindings for TTY mode
	 ("M-[ 1 ; 6 n" . mc/mark-next-like-this)
	 ("M-[ 1 ; 6 l" . mc/mark-previous-like-this)
	 ("C-c M-[ 1 ; 6 l" . mc/mark-all-like-this)
	 ("C-c M-[ 1 ; 6 n" . mc/mark-more-like-this-extended))

  :init
  (progn
    (defun nispio/fake-cursor-at-point ()
      (interactive)
      (mc/create-fake-cursor-at-point))
    (bind-key "C-c C-SPC" 'nispio/fake-cursor-at-point)

    ;; Add extended interoperability between phi-search and multiple cursors
    ;; (source: https://github.com/knu/phi-search-mc.el)
    (when (package-installed-p 'phi-search)
      (use-package phi-search-mc
	:ensure t
	:init (phi-search-mc/setup-keys)))))

(bind-keys
 ("C-c c" . comment-region)
 ("C-c u" . uncomment-region))

(define-key ctl-x-map (kbd "<f5>") 'revert-buffer)
(define-key ctl-x-map (kbd "<f6>") 'add-file-local-variable)

;; Unfortunately, multiple-cursors falls short on rectangular selection
;;   so I use rect-mark.el to fill in the gaps for now
;; (source: http://www.emacswiki.org/emacs/rect-mark.el)
(use-package phi-rectangle
  :ensure t
  :bind (("C-c C-SPC" . phi-rectangle-set-mark-command)
	 ("C-w" . phi-rectangle-kill-region)
	 ("M-w" . phi-rectangle-kill-ring-save)
	 ("C-y" . phi-rectangle-yank)))

;; Add support for editing matlab files
;; (source: http://matlab-emacs.cvs.sourceforge.net/viewvc/matlab-emacs/matlab-emacs/?view=tar)
(add-to-list 'load-path "~/.emacs.d/site-lisp/matlab-emacs")
(load-library "matlab-load")
(setq matlab-comment-column 50)

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

;; Set up auto-complete
;; (source: https://github.com/auto-complete/auto-complete)
(add-to-list 'load-path "~/.emacs.d/site-lisp/auto-complete")
(add-to-list 'load-path "~/.emacs.d/site-lisp/auto-complete/lib/popup")
(add-to-list 'load-path "~/.emacs.d/site-lisp/auto-complete/lib/fuzzy")
(add-to-list 'load-path "~/.emacs.d/site-lisp/auto-complete/lib/erc")
(require 'auto-complete-config)
(ac-config-default)
;; start after 3 characters were typed
(setq ac-auto-start 3)
;; show menu immediately...
(setq ac-auto-show-menu t)
;; explicit call to auto-complete
(define-key ac-mode-map (kbd "C-.") 'auto-complete)
;; Allow auto-complete with matlab-mode
(setq ac-modes (cons 'matlab-mode ac-modes))
;; (add-hook 'matlab-mode-hook 'nispio/auto-complete-mode)
;;(add-hook 'matlab-mode-hook 'auto-complete-mode)


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

;; ;; Add sublimity mode for mini-map
;; ;; (source: https://github.com/zk-phi/sublimity.git)
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/sublimity")
;; (require 'sublimity)
;; (require 'sublimity-scroll)
;; (require 'sublimity-map)

;; ;; Add support for Chrome extension "Edit with Emacs"
;; ;; (source: https://github.com/stsquad/emacs_chrome.git)
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/emacs_chrome/servers")
;; (when (require 'edit-server nil t)
;;     (setq edit-server-new-frame nil)
;;     (add-hook 'edit-server-start-hook 'flyspell-mode)
;;     (add-hook 'edit-server-start-hook 'visual-line-mode)
;;     (add-hook 'edit-server-started-hook 'delete-other-windows)
;;     (add-hook 'edit-server-buffer-closed-hook 'delete-window)
;;     (edit-server-start))

;; SrSpeedbar allows a speedbar that is "docked" in the current frame
(require 'sr-speedbar)
(global-set-key (kbd "C-c M-SPC") 'sr-speedbar-toggle)
(global-set-key (kbd "C-c C-g w") 'sr-speedbar-select-window)

;; Configure GDB for debugging
(setq gdb-show-main t)
(setq gdb-many-windows t)
(put 'debug-command 'safe-local-variable 'stringp)

;; Call the compiler and save the compile command when in C
(defun nispio/compile-c (&optional arg)
  (interactive "P")
  (when arg (makunbound 'compile-command))
  (unless (boundp 'compile-command)
	(let* ((src-file (file-name-nondirectory buffer-file-name))
		   (src-ext (file-name-extension src-file))
		   (src-base (file-name-base src-file))
		   (src-compiler (if (string= src-ext "cpp") "g++" "gcc"))
		   (my-guess (concat src-compiler " -g3 -ggdb -o " src-base " " src-file)))
	  (setq-local compile-command (read-string "Compile command: " my-guess))
	  (when (y-or-n-p "Save file-local variable compile-command?")
		(add-file-local-variable 'compile-command compile-command))))
  (compile compile-command t))

;; Run the debugger and save the debug command when in C
(defun nispio/run-debugger (&optional arg)
  (interactive "P")
  (when arg (makunbound 'debug-command))
  (unless (boundp 'debug-command)
	(let* ((src-base (file-name-base buffer-file-name))
		   (my-guess (concat "gdb -i=mi " src-base)))
	  (setq-local debug-command (read-string "Run gdb (like this): " my-guess))
	  (when (y-or-n-p "Save file-local variable debug-command?")
		(add-file-local-variable 'debug-command debug-command))))
  (sr-speedbar-open)
  (gdb debug-command))

;; Run debugger in another (maximized) frame
(defun nispio/debug-other-frame ()
  (interactive)
  (select-frame (make-frame))
  (setq nispio/fullscreen-p t)
  (nispio/maximize-frame)
  (nispio/run-debugger))

;; Set a Watch Expression in the debugger
(defun nispio/gud-watch-expression (&optional arg)
  (interactive "P")
  (if arg
	  (call-interactively gud-watch)
	(gud-watch '(4))))

;; Set up C-mode specific keybindings
(defun nispio/c-mode-keys-hook ()
  (local-set-key (kbd "C-c C-c") 'nispio/compile-c)
  (local-set-key (kbd "<f5>") 'nispio/run-debugger)
  (local-set-key (kbd "<S-f5>") 'nispio/debug-other-frame))
(add-hook 'c-mode-common-hook 'nispio/c-mode-keys-hook)

(defun nispio/set-clear-breakpoint (&optional arg)
  "Set/clear breakpoint on current line"
  (interactive "P")
  (if (or (buffer-file-name) (derived-mode-p 'gdb-disassembly-mode))
	  (if (eq (car (fringe-bitmaps-at-pos (point))) 'breakpoint)
		  (gud-remove nil)
		(gud-break nil))))

(defun nispio/toggle-breakpoint (&optional arg)
  "Enable/disable breakpoint on current line"
  (interactive "P")
  (save-excursion
	(forward-line 0)
	(dolist (overlay (overlays-in (point) (point)))
	  (when (overlay-get overlay 'put-break)
		(setq obj (overlay-get overlay 'before-string))))
	(when (and (boundp 'obj) (stringp obj))
	  (gud-basic-call
	   (concat
		(if (get-text-property 0 'gdb-enabled obj)
			"-break-disable "
		  "-break-enable ")
		(get-text-property 0 'gdb-bptno obj))))))

(defun nispio/clear-all-breakpoints (&optional arg)
  "Clear all breakpoints"
  (interactive "P")
  (gud-basic-call "delete breakpoints"))

(defun nispio/enable-breakpoints (&optional arg)
  "Enable/Disable all breakpoints at once"
  (interactive "P")
  (gud-basic-call "enable breakpoints"))

(defun nispio/disable-breakpoints (&optional arg)
  "Enable/Disable all breakpoints at once"
  (interactive "P")
  (gud-basic-call "disable breakpoints"))

(defun nispio/stop-debugging (&optional arg)
  "Kill the process being debugged"
  (interactive "P")
  (kill-buffer gud-comint-buffer))

(defun nispio/mouse-toggle-breakpoint (event)
  "Set/clear breakpoint in left fringe/margin at mouse click.
If not in a source or disassembly buffer just set point."
  (interactive "e")
  (mouse-minibuffer-check event)
  (let ((posn (event-end event)))
    (with-selected-window (posn-window posn)
      (if (or (buffer-file-name) (derived-mode-p 'gdb-disassembly-mode))
	  (if (numberp (posn-point posn))
	      (save-excursion
		(goto-char (posn-point posn))
		(if (eq (car (fringe-bitmaps-at-pos (posn-point posn)))
			    'breakpoint)
		    (gud-remove nil)
		  (gud-break nil)))))
      (posn-set-point posn))))


;; Set up GUD specific keybindings
(defun nispio/gdb-mode-keys-hook ()
  ;; Mouse Actions
  (define-key gud-minor-mode-map [left-margin mouse-1] 'nispio/mouse-toggle-breakpoint)
  (define-key gud-minor-mode-map [double-mouse-1] 'gud-until)
  (define-key gud-minor-mode-map [mouse-3] 'mouse-set-point)
  (define-key gud-minor-mode-map [double-mouse-3] 'gud-print)
  (define-key gud-minor-mode-map [mouse-2] 'gud-watch)
  ;; Keyboard Actions
  (define-key gud-minor-mode-map [f5] 'gud-go) ; "Continue"
  (define-key gud-minor-mode-map [f6] 'gud-watch)
  (define-key gud-minor-mode-map [S-f6] 'gud-until)
  (define-key gud-minor-mode-map [S-f5] 'nispio/stop-debugging) ; "Stop Debugging"
  (define-key gud-minor-mode-map [C-S-f5] 'gud-run) ; "Restart""
  (define-key gud-minor-mode-map [f8] 'nispio/enable-breakpoints) ; "Toggle Breakpoint"
  (define-key gud-minor-mode-map [S-f8] 'nispio/disable-breakpoints) ; "Toggle Breakpoint"
  (define-key gud-minor-mode-map [f9] 'nispio/set-clear-breakpoint) ; "Toggle Breakpoint"
  (define-key gud-minor-mode-map [C-f9] 'nispio/toggle-breakpoint) ; "Disable/Enable Breakpoint"
  (define-key gud-minor-mode-map [C-S-f9] 'nispio/clear-all-breakpoints) ; "Delete all Breakpoints"
  (define-key gud-minor-mode-map [S-f9] 'nispio/gud-watch-expression) ; "Quick Watch"
  (define-key gud-minor-mode-map [f10] 'gud-next) ; "Step over"
  (define-key gud-minor-mode-map [S-f10] 'gud-stepi)
  (define-key gud-minor-mode-map [f11] 'gud-step) ; "Step into""
  (define-key gud-minor-mode-map [S-f11] 'gud-finish)) ; "Step out of"
;; gud-jump	      ; Set execution address to current line
;; gud-refresh	  ; Fix up a possibly garbled display, and redraw the arrow
;; gud-tbreak	  ; Set temporary breakpoint at current line
(add-hook 'gdb-mode-hook 'nispio/gdb-mode-keys-hook)

;; Set up hotkeys for transitioning between windows in gdb
;; (source: http://markshroyer.com/2012/11/emacs-gdb-keyboard-navigation/)
(defun gdb-comint-buffer-name ()
  (buffer-name gud-comint-buffer))
(defun gdb-source-buffer-name ()
  (buffer-name (window-buffer gdb-source-window)))

(defun gdb-select-window (header)
  "Switch directly to the specified GDB window.
Moves the cursor to the requested window, switching between
`gdb-many-windows' \"tabs\" if necessary in order to get there.

Recognized window header names are: 'comint, 'locals, 'registers,
'stack, 'breakpoints, 'threads, and 'source."

  (interactive "Sheader: ")

  (let* ((header-alternate (case header
                             ('locals      'registers)
                             ('registers   'locals)
                             ('breakpoints 'threads)
                             ('threads     'breakpoints)))
         (buffer (intern (concat "gdb-" (symbol-name header) "-buffer")))
         (buffer-names (mapcar (lambda (header)
                                 (funcall (intern (concat "gdb-"
                                                          (symbol-name header)
                                                          "-buffer-name"))))
                               (if (null header-alternate)
                                   (list header)
                                 (list header header-alternate))))
         (window (if (eql header 'source)
                     gdb-source-window
                   (or (get-buffer-window (car buffer-names))
                       (when (not (null (cadr buffer-names)))
                         (get-buffer-window (cadr buffer-names)))))))

    (when (not (null window))
      (let ((was-dedicated (window-dedicated-p window)))
        (select-window window)
        (set-window-dedicated-p window nil)
        (when (member header '(locals registers breakpoints threads))
          (switch-to-buffer (gdb-get-buffer-create buffer))
          (setq header-line-format (gdb-set-header buffer)))
        (set-window-dedicated-p window was-dedicated))
      t)))

;; Use global keybindings for the window selection functions so that they
;; work from the source window too...
(mapcar (lambda (setting)
          (lexical-let ((key    (car setting))
                        (header (cdr setting)))
            (global-set-key (concat "\C-c\C-g" key) #'(lambda ()
                                                        (interactive)
                                                        (gdb-select-window header)))))
        '(("c" . comint)
          ("l" . locals)
          ("r" . registers)
          ("u" . source)
          ("s" . stack)
          ("b" . breakpoints)
          ("t" . threads)))


;; Settings modified via the Customize interface get their own file
(setq custom-file "~/.emacs.d/settings.el")
(load custom-file)
