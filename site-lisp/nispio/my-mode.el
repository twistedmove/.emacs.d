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

;; If not in a TTY, Unbind C-m so that we can use it elsewhere
(if (not (display-graphic-p))
    (setq tty-keys t)
  ;; (define-key input-decode-map [?\C-m] [C-m])
  ;; (define-key input-decode-map [?\C-i] [C-i])
  ;; (define-key input-decode-map [?\C-\[] [C-\[])
  (setq tty-keys nil))

;(define-key my-map [remap digit-argument] 'my-key-do-nothing)
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



(provide 'nispio/my-mode)


