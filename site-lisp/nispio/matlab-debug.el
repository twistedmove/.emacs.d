;; Source: http://nullprogram.com/blog/2013/02/06    

;; Matlab Debug Mode

(defun nispio/matlab-mouse-breakpoint (event)
  "Set breakpoint in left fringe/margin at mouse click.
If not in a source or disassembly buffer just set point."
  (interactive "e")
  (mouse-minibuffer-check event)
  (let ((posn (event-end event)))
    (with-selected-window (posn-window posn)
      (if (or (buffer-file-name) (derived-mode-p 'gdb-disassembly-mode))
	  (if (numberp (posn-point posn))
	      (save-excursion
		(goto-char (posn-point posn))
		(gud-break nil))))
      (posn-set-point posn))))


(defvar matlab-debug-map 
;  (setq matlab-debug-map
  (let ((map (make-sparse-keymap)))
        (define-key map [left-margin mouse-1] 'nispio/matlab-mouse-breakpoint) ; "dbstop"
	(define-key map (kbd "<f9>")   'gud-break)  ; Set breakpoint "dbstop"
	(define-key map (kbd "C-<f9>") 'gud-remove) ; Remove breakpoint "dbclear"
	(define-key map (kbd "<f11>")  'gud-step)   ; Step in "dbstep in""
	(define-key map (kbd "<f10>")  'gud-next)   ; Step over "dbstep %p"
	(define-key map (kbd "<f5>")   'gud-cont)   ; Continue "dbcont"
	(define-key map (kbd "S-<f5>") 'gud-finish) ; Quit debugging "dbquit"
	(define-key map (kbd "<f7>")   'gud-up)	    ; Up N stack frames "dbup %p"
	(define-key map (kbd "S-<f7>") 'gud-down)   ; Down N stack frames "dbdown %p"
	(define-key map (kbd "S-<f9>") 'gud-print)  ; Evaluate expression at point. "%e"
    map)
;  )
  "Keymap for debugging matlab within emacs.")

;(prin1-to-string matlab-debug-map)

(define-minor-mode matlab-debug-mode
  "A minor mode for debugging matlab within emacs."
  nil
  " ml-dbg"
  matlab-debug-map)

(provide 'nispio/matlab-debug)

;; (progn
;;   (define-key matlab-debug-map [left-margin mouse-1] 'nispio/mouse-toggle-breakpoint)
;;   (define-key matlab-debug-map [double-mouse-1] 'gud-until)
;;   (define-key matlab-debug-map [mouse-3] 'mouse-set-point)
;;   (define-key matlab-debug-map [double-mouse-3] 'gud-print)
;;   (define-key matlab-debug-map [mouse-2] 'gud-watch)
;;   (define-key matlab-debug-map [f5] 'gud-go)
;;   (define-key matlab-debug-map [f6] 'gud-watch)
;;   (define-key matlab-debug-map [S-f6] 'gud-until)
;;   (define-key matlab-debug-map [S-f5] 'nispio/stop-debugging) ; "Stop Debugging"
;;   (define-key matlab-debug-map [C-S-f5] 'gud-run) ;"Restart"
;;   (define-key matlab-debug-map [f8] 'nispio/enable-breakpoints) ; "Toggle Breakpoint"
;;   (define-key matlab-debug-map [S-f8] 'nispio/disable-breakpoints) ; "Toggle Breakpoint"
;;   (define-key matlab-debug-map [f9] 'nispio/set-clear-breakpoint) ; "Toggle Breakpoint"
;;   (define-key matlab-debug-map [C-f9] 'nispio/toggle-breakpoint) ; "Disable/Enable Breakpoint"
;;   (define-key matlab-debug-map [C-S-f9] 'nispio/clear-all-breakpoints) ; "Delete all Breakpoints"
;;   (define-key matlab-debug-map [S-f9] 'nispio/gud-watch-expression) ; "Quick Watch"
;;   (define-key matlab-debug-map [f10] 'gud-next) ;"Step over"
;;   (define-key matlab-debug-map [S-f10] 'gud-stepi)
;;   (define-key matlab-debug-map [f11] 'gud-step) ;"Step into""
;;   (define-key matlab-debug-map [S-f11] 'gud-finish) ;"Step out of"
;;   matlab-debug-map)

;; (bind-keys
;;  ("<f9>" . gud-break)  ; Set breakpoint "dbstop"
;;  ("C-<f9>" . gud-remove) ; Remove breakpoint "dbclear"
;;  ("<f11>" . gud-step)   ; Step in "dbstep in""
;;  ("<f10>" . gud-next)   ; Step over "dbstep %p"
;;  ("<f5>" . gud-cont)   ; Continue "dbcont"
;;  ("S-<f5>" . gud-finish) ; Quit debugging "dbquit"
;;  ("<f7>" . gud-up)	    ; Up N stack frames "dbup %p"
;;  ("S-<f7>" . gud-down)   ; Down N stack frames "dbdown %p"
;;  ("S-<f9>" . gud-print)  ; Evaluate expression at point. "%e"
;;  ("<left-margin> <mouse-1>" . nispio/matlab-mouse-breakpoint))
