;;;###autoload
(defun make-bare-bones-map ()
  (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-\\") 'other-window)
        (define-key map (kbd "M-1") 'delete-other-windows)
        (define-key map (kbd "C-c c") 'comment-region)
        (define-key map (kbd "C-c u") 'uncomment-region)
;        (define-key map (kbd "") nil)
    map)
  )

(define-key dired-mode-map (kbd "M-s d i") 'nispio/find-iname-dired)
(define-key dired-mode-map (kbd "M-s d r") 'nispio/find-regexp-dired)

(defvar bare-bones-map (make-bare-bones-map)
  "Bare-bones keymap with my essential keybindings")

;(setq bare-bones-map (make-bare-bones-map))

(define-minor-mode init-emergency-mode
  "A minor mode for init-file emergencies"
  nil
  " EMERGENCY"
  bare-bones-map)



(provide 'init-emergency)

(mapconcat 'identity (dired-get-marked-files) " ")
