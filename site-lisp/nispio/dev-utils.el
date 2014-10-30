(require 'nispio/misc-utils)

;; Set up indenting in C/C++
(setq c-default-style "linux")
(setq-default c-basic-offset 4)
(setq-default tab-width 4)
(c-set-offset 'inline-open 0)

(setq comint-scroll-to-bottom-on-input t)
(setq-default comint-move-point-for-output 'others)

;; Set up C-mode specific keybindings
(defun nispio/c-mode-keys-hook ()
  (local-set-key (kbd "C-c C-c") 'nispio/compile-c)
  (local-set-key (kbd "<f5>") 'nispio/run-debugger)
  (local-set-key (kbd "<S-f5>") 'nispio/debug-other-frame))
(add-hook 'c-mode-common-hook 'nispio/c-mode-keys-hook)

;; Shortcut for opening and closing braces in c-mode
(defun nispio/insert-braces ()
  (interactive)
  (execute-kbd-macro '[return 123 tab return 125 tab 16 tab]))
(defun nispio/insert-braces-hook ()
  (local-set-key (kbd "<C-m>") 'nispio/insert-braces))
(add-hook 'c-mode-common-hook 'nispio/insert-braces-hook)

;; Make C mode use C++-style commenting
(add-hook 'c-mode-hook (lambda () (setq comment-start "// " comment-end  "")))

;; Configure GDB for debugging
(setq gdb-show-main t)
;;(setq gdb-many-windows t)
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
  (unless (window-resizable-p nil 1)
	(split-window-below))
  (gdb debug-command)
  (with-current-buffer gud-comint-buffer
	(nispio/set-window-size nil 12)
	(set-window-dedicated-p nil t)
	(when (display-graphic-p)
	  (tool-bar-mode 1)
	  (add-hook 'kill-buffer-hook (lambda () (tool-bar-mode -1)) t t)))
  (add-hook 'kill-buffer-hook 'nispio/delete-window-maybe t t))

(defun nispio/stop-debugging ()
  (interactive)
  (let* ((buffer (get-buffer gud-comint-buffer))
		 (proc (and buffer (get-buffer-process buffer))))
	(when (and proc (process-live-p proc))
	  (message "Killing GUD comint buffer process...")
	  (kill-process proc)
	  (while (process-live-p proc) nil)
	  (message "Exit debugger"))
	(kill-buffer buffer)))

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

(defun nispio/attach (pid)
  "Attach gdb to a running process"
  (interactive "nAttach to process id: ")
  (comint-send-string gud-comint-buffer (format "attach %d\n" pid))
  (sit-for 3)
  (gud-cont nil))

;; Set up GUD specific keybindings
(defun nispio/gdb-mode-keys-hook ()
  ;; Mouse Actions
  (define-key gud-minor-mode-map [left-margin mouse-1] 'nispio/mouse-toggle-breakpoint)
  (define-key gud-minor-mode-map [double-mouse-1] 'gud-until)
  (define-key gud-minor-mode-map [mouse-3] 'mouse-set-point)
  (define-key gud-minor-mode-map [double-mouse-3] 'gud-print)
  (define-key gud-minor-mode-map [mouse-2] 'gud-watch)
  ;; Keyboard Actions
  (define-key gud-minor-mode-map [f5] 'gud-cont) ; "Continue"
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



(defun nispio/setup-gud-toolbar ()
  ;; Modify existing buttons in the GUD toolbar
  (let ((menu gud-tool-bar-map))
	(dolist (x '((go :visible nil)
				 (cont :visible t)
				 (cont :help "Continue (gud-cont)" )
				 (next menu-item "Step Over")
				 (next :help "Step Over (gud-next)")
				 (step menu-item "Step Into")
				 (step :help "Step Into (gud-step)")
				 (finish :help "Step Out (gud-finish)")
				 (finish menu-item "Step Out")
				 (finish :vert-only nil)
				 (up :vert-only t)
				 (down :vert-only t))
			   gud-tool-bar-map)
	  (nispio/menu-item-property menu (car x) (cadr x) (caddr x)))

	;; Add Attach to Process button to GUD toolbar
	(define-key-after menu [attach]
	  '(menu-item "Attach" nispio/attach
				  :help "Attach To Running Process"
				  :enable (not gud-running)
				  :image (image :type xpm :file "attach.xpm")
				  :vert-only t)
	  'watch)

	;; Add Stop Debugging button to GUD toolbar
	(define-key-after menu [exit]
	  '(menu-item "Quit Debugging" nispio/stop-debugging
				  :help (let ((key (where-is-internal 'nispio/stop-debugging nil t)))
						  (format "Exit GUD %s" (key-description key)))
				  :image (image :type xpm :file "exit.xpm")
				  :vert-only t))))

(add-hook 'gdb-mode-hook 'nispio/setup-gud-toolbar)




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

(defvar nispio/gdb-window-map (make-sparse-keymap)
  "Keymap for selecting GDB windows")

;; Use global keybindings for the window selection functions so that they
;; work from the source window too...
(let ((map nispio/gdb-window-map))
  (mapcar (lambda (el)
			(lexical-let ((key    (car el))
						  (header (cdr el)))
			  (define-key map (kbd key) #'(lambda ()
											(interactive)
											(gdb-select-window header)))))
		  '(("c" . comint)
			("l" . locals)
			("r" . registers)
			("u" . source)
			("s" . stack)
			("b" . breakpoints)
			("t" . threads))))



(require 'semantic/ia)
(require 'semantic/bovine/gcc)
(semantic-mode 1)
(define-key my-map (kbd "H-j")'semantic-ia-fast-jump)



(provide 'nispio/dev-utils)
