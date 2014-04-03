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
(setq x-stretch-cursor t)				; Cursor as wide as the glyph under it

;; Disable useless UI features
(when window-system
  (scroll-bar-mode 0)                   ; Disable scroll bars
  (tool-bar-mode 0)                     ; Disable toolbar
  (tooltip-mode 0))						; Disable tooltips

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
(setq linum-format "%3d")				; Right-aligned line numbers with width 3
(fringe-mode '(nil . 0))				; Left fringes only

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

;; Easier navigation between windows/frames
(defun nispio/other-window (&optional arg)
  "With prefix argument, select the next frame. Otherwise, select the next window"
  (interactive "P")
  (if arg (other-frame 1) (other-window 1)))
(global-set-key (kbd "C-\\") 'nispio/other-window)

;; Other keybindings
(global-set-key (kbd "C-c c") 'comment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-q") 'kill-buffer-and-window)
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

;; Make C mode use C++-style commenting
(add-hook 'c-mode-hook (lambda () (setq comment-start "// " comment-end   "")))

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

;; Window manipulations:
(global-set-key (kbd "C-S-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "C-S-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-S-<up>")    'shrink-window)
(global-set-key (kbd "C-S-<down>")  'enlarge-window)
(global-set-key (kbd "M-o 6 d")     'shrink-window-horizontally)
(global-set-key (kbd "M-o 6 c")     'enlarge-window-horizontally)
(global-set-key (kbd "M-o 6 a")     'shrink-window)
(global-set-key (kbd "M-o 6 b")     'enlarge-window)

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
;; Toggle "fullscreen" by maximizing or restoring the current frame.
  (interactive)
  (setq nispio/fullscreen-p (not nispio/fullscreen-p))
  (if nispio/fullscreen-p (nispio/restore-frame) (nispio/maximize-frame)))
(global-set-key (kbd "<f12>") 'nispio/toggle-fullscreen)
(global-set-key (kbd "<S-f12>") 'delete-frame)

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
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-S-c C-<") 'mc/mark-all-in-region)
;(global-set-key (kbd "C-S-c C->") 'mc/mark-all-regexp-in-region)
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

;; Add extended interoperability between phi-search and multiple cursors
;; (source: https://github.com/knu/phi-search-mc.el.git)
(add-to-list 'load-path "~/.emacs.d/site-lisp/phi-search-mc")
(require 'phi-search-mc)
(phi-search-mc/setup-keys)

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

;; Set up auto-complete
;; (source: https://github.com/auto-complete/auto-complete.git)
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
 '(safe-local-variable-values (quote ((visual-line-mode . t) (auto-fill-mode . 0))))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(put 'dired-find-alternate-file 'disabled nil)
