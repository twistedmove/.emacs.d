;;;; .emacs

(message "Begin initialization file init.el")
(switch-to-buffer "*Messages*")

;; Start the server
(require 'server)
(unless (server-running-p)
  (server-start))

;; parse command line arguments
(setq init-file-broken-p (member "--broken" command-line-args))
(setq command-line-args (delete "--broken" command-line-args))

(when init-file-broken-p
    (message "Hooray! It's broken!"))

;; Set load paths for lisp files
(add-to-list 'load-path "~/.emacs.d/site-lisp")
(add-to-list 'load-path "~/.emacs.d/nispio")

;; Basic emacs settings
(show-paren-mode 1)						; Show matching parenthesis
(setq-default truncate-lines t)         ; Truncate lines by default

;; Basic emacs settings
(global-font-lock-mode 1)               ; Enable syntax highlighting
(setq inhibit-startup-screen t)         ; Disable splash screen
(setq visible-bell t)                   ; Disable system beep
(setq transient-mark-mode t)            ; Enable visual feedback on selections
(column-number-mode t)                  ; Show column number on mode line
(show-paren-mode)                       ; Show matching parenthesis
(electric-pair-mode 1)                  ; Enable automatic bracket closing
(setq x-stretch-cursor t)               ; Cursor as wide as the glyph under it

;; Further customization
(setq scroll-step 1)                    ; Only scroll by one line at top/bottom
(setq-default truncate-lines t)         ; Truncate lines by default
(setq display-time-day-and-date t)      ; Dispaly date along with time in status bar
(display-time)                          ; Display date and time in status bar
(setq require-final-newline t)          ; Always end a file with a newline
(setq frame-title-format "emacs - %b")  ; Set frame title to "emacs - <buffer name>"

(load-file "~/.emacs.d/nispio/init-packages.el")
(load-file "~/.emacs.d/nispio/xmidas.el")
(load-file "~/.emacs.d/nispio/dsv-to-orgtbl.el")
(load-file "~/.emacs.d/nispio/org-table-header.el")
(load-file "~/.emacs.d/nispio/org-table-align.el")

;; Use a more powerful alternative to ido-mode's flex matching.
;; (source: https://github.com/lewang/flx.git)
(use-package flx-ido :ensure t)

;; Use "ido" completion wherever possible
;; (source: https://github.com/DarwinAwardWinner/ido-ubiquitous)
(use-package ido-ubiquitous
  :ensure t
  :init
  (progn
    (iswitchb-mode 1)
    (ido-mode 1)                        ; Enable ido mode (interactively do)
    (ido-ubiquitous-mode 1)))           ; Enable ido mode almost everywhere
  
(bind-key "M-s n" 'find-name-dired)
(bind-key* "C-h B" 'describe-personal-keybindings)

;; Use incremental completion and selection narrowing
;; (source: https://github.com/emacs-helm/helm)
(use-package helm-config
  :ensure helm
  :init
  (progn
    (require 'helm)
    ;; (require 'helm-plugin)

    ;; Helm interface for describe bindings
    ;; (source: https://github.com/emacs-helm/helm-descbinds)
    (use-package helm-descbinds
      :ensure t
      :bind ("C-h b" . helm-descbinds))

    ;; Turn on follow mode when using multi-occur
    (require 'helm-regexp)
    (eval-after-load "helm-regexp"
      '(setq helm-source-moccur
        (helm-make-source "Moccur" 'helm-source-multi-occur :follow 1)))

    ;; (source: http://stackoverflow.com/q/14726601)
    (defun nispio/helm-moccur-buffers ()
      "multi-occur in all buffers backed by files."
      (interactive)
      (helm-multi-occur
       (delq nil
         (mapcar (lambda (b)
               (when (buffer-file-name b) (buffer-name b)))
             (buffer-list)))))


    ;; (source: http://emacs.stackexchange.com/a/650/93)
    (defun nispio/helm-full-frame ()
      (interactive)
      (with-selected-window (helm-window)
    (delete-other-windows)))

    (bind-key "M-1" 'nispio/helm-full-frame helm-map)

    (bind-key "C-c M-x" 'helm-M-x)
    (bind-key "C-h a" 'helm-apropos)
    (bind-key "C-h p" 'helm-list-elisp-packages)

    (bind-key "M-s b" 'nispio/helm-moccur-buffers)
    (bind-key "M-s a" 'helm-do-grep)
    (bind-key "M-s o" 'helm-occur)
    (bind-key "M-s r" 'helm-register)))


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
	(helm-projectile-on)))


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

;; Easier navigation between windows/frames
(defun nispio/other-window (&optional arg)
  "With prefix argument, select the next frame. Otherwise, select the next window"
  (interactive "P")
  (let* ((N (or (and (consp arg) -1) arg 1)))
    (other-window N t)
    (select-frame-set-input-focus (selected-frame))))
(bind-key* "C-\\" 'nispio/other-window)

(defun nispio/show-prefix-arg (&optional arg)
  (interactive "P")
  (if (/= 0 (safe-length arg))
      (message "Prefix: %s (%d)" arg (safe-length arg))
    (message "Argument: %s (%d)" arg (safe-length arg))
  ))

(defun nispio/buffer-file-name ()
  "Display the name of the file backing the current buffer"
  (interactive)
  (message (or buffer-file-name "no file"))
  buffer-file-name)
(define-key ctl-x-map (kbd "<f1>") 'nispio/buffer-file-name)



(defun nispio/insert-key-description (key &optional arg)
"Capture a keybinding directly from the keyboard and insert its string
representation at point. With optional ARG, display the key description in the
minibuffer instead of inserting it at point."
  (interactive "k\nP")
  (let ((desc (key-description key)))
	(if arg (message desc) (insert desc))))
(bind-key* "C-h C-k" 'nispio/insert-key-description)

;(use-package thing-cmds :ensure t)

;; Other keybindings
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-0") 'kill-buffer-and-window)

;; Extend dired functionality
(use-package dired+
  :ensure t
  :config
  (progn
    (use-package image-dired :ensure t)
    
    (require 'dired-x)
    ;; Command to open all marked files at once
    (bind-keys :map dired-mode-map
           ("F" . dired-do-find-marked-files)
           ("/" . phi-search))
    ;; When opening a directory in dired, reuse the current buffer
    (diredp-toggle-find-file-reuse-dir 1)))


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

;; Use ibuffer in place of the standard list-buffers command
(bind-key "C-x C-b" 'ibuffer)


;; Easily re-arrange buffers within the frame
;; (source: http://www.emacswiki.org/emacs/download/buffer-move.el)
(use-package buffer-move
  :ensure t
  :bind
  (("C-c <C-up>" . buf-move-up)
   ("C-c <C-down>" . buf-move-down)
   ("C-c <C-left>" . buf-move-left)
   ("C-c <C-right>" . buf-move-right)))

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
  (use-package preview-latex)
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
  :bind (("C-c M-SPC" . sr-speedbar-toggle)
     ("C-c C-g w" . sr-speedbar-select-window)))

(load-file "~/.emacs.d/nispio/init-devel.el")

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
;; gud-jump       ; Set execution address to current line
;; gud-refresh    ; Fix up a possibly garbled display, and redraw the arrow
;; gud-tbreak     ; Set temporary breakpoint at current line
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
(if window-system
    (setq custom-file "~/.emacs.d/nispio/settings.el")
  (setq custom-file "~/.emacs.d/nispio/settings-tty.el"))
(load custom-file)
