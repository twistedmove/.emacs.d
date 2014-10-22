(require 'easy-mmode)

(defvar my-map (make-sparse-keymap)
  "Keymap for my personal key bindings")

(define-minor-mode my-mode
  "A minor mode which provides my personal key bindings."
  nil " MY" my-map)

(defun my-mode-maybe ()
  "Enable minor my-mode in the current buffer, unless in minibuffer"
  (if (not (minibufferp (current-buffer)))
      (my-mode 1)))

(define-global-minor-mode my-global-mode my-mode my-mode-maybe)

(defun enable-my-global-mode ()
  "Command to enable my-global-mode"
  (interactive)
  (my-global-mode 1)
  (message "my-global-mode enabled"))

(defun disable-my-global-mode ()
  "Command to disable my-global-mode"
  (interactive)
  (my-global-mode -1)
  (message "my-global-mode disabled"))

(defun my-key-do-nothing ()
  (interactive)
  (message "This key is not bound"))

(define-key my-map [remap digit-argument] 'my-key-do-nothing)
(define-key my-map [remap list-buffers] 'ibuffer)
(define-key my-map (kbd "C-M-&") 'disable-my-global-mode)
(define-key my-map (kbd "C--") 'delete-window)
(define-key my-map (kbd "C-0") 'delete-window)
(define-key my-map (kbd "M-0") 'delete-window)
(define-key my-map (kbd "M-1") 'delete-other-windows)
(define-key my-map (kbd "C-\\") 'nispio/other-window)
(define-key my-map (kbd "C-x <f1>") 'nispio/buffer-file-name)
(define-key my-map (kbd "<f11>") 'nispio/toggle-fullscreen)
(define-key my-map (kbd "C-j") 'newline-and-indent)


(defun nispio/other-window (&optional arg)
  "Cycle through windows. With prefix arg, cycle backwards"
  (interactive "P")
  (let* ((N (or (and (consp arg) -1) arg 1)))
    (other-window N 'visible)
    (select-frame-set-input-focus (selected-frame))))

(defun nispio/buffer-file-name ()
  "Display the name of the file backing the current buffer"
  (interactive)
  (message (or buffer-file-name "no file"))
  buffer-file-name)

;; Custom function to toggle fullscreen by maximizing or restoring the current frame.
(defvar nispio/fullscreen-p t "Check if fullscreen is on or off")

(defun nispio/restore-frame ()
  (if (fboundp 'w32-send-sys-command) (w32-send-sys-command 61728)
    (set-frame-parameter nil 'width 82)
	(set-frame-parameter nil 'fullscreen 'fullheight)))
(defun nispio/maximize-frame ()
  (if (fboundp 'w32-send-sys-command) (w32-send-sys-command 61488)
    (set-frame-parameter nil 'fullscreen 'fullboth)))
(defun nispio/toggle-fullscreen (&optional DEL)
  "Toggle \"fullscreen\" by maximizing or restoring the current frame.
  If optional argument DEL is non-nil, delete the current frame."
  (interactive "P")
  (if DEL (delete-frame)
	(setq nispio/fullscreen-p (not nispio/fullscreen-p))
	(if nispio/fullscreen-p
		(nispio/restore-frame)
	  (nispio/maximize-frame))))



(provide 'nispio/my-mode)


