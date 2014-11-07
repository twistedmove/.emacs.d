
;; (source: http://ergoemacs.org/emacs/emacs_copy_rectangle_text_to_clipboard.html)
(defun nispio/copy-rectangle-to-kill-ring (start end)
  "Saves a rectangle to the normal kill ring. Not suitable for yank-rectangle."
  (interactive "r")
  (let ((lines (extract-rectangle start end)))
    (with-temp-buffer
      (while lines ;; insert-rectangle, but without the unneeded stuff
        ;; (most importantly no push-mark)
        (insert-for-yank (car lines))
        (insert "\n")
        (setq lines (cdr lines)))
      (kill-ring-save (point-min) (point-max)))))

;; (adapted from: http://emacs.stackexchange.com/a/3174/93)
(defun nispio/push-killed-rectangle-to-kill-ring (&optional arg)
  "Saves a rectangle to the normal kill ring. Not suitable for yank-rectangle."
  (interactive "*P")
  (let ((lines killed-rectangle))
    (with-temp-buffer
      (while lines
        (insert-for-yank (car lines))
        (insert "\n")
        (setq lines (cdr lines)))
      (kill-ring-save (point-min) (point-max)))
	(setq phi-rectangle--last-killed-is-rectangle nil)))

(defun nispio/yank-rectangle-from-kill-ring (&optional arg)
  "Copy the current killed rectangle to the kill ring and"
  (interactive "*P")
  (nispio/push-killed-rectangle-to-kill-ring)
  (yank arg))

;; (defun nispio/region-to-phi-rectangle (start end &optional fill)
;;   (interactive "r\nP")
;;   (unless (fboundp 'phi-rectangle-set-mark-command)
;; 	(error "This command require the package `phi-rectangle'"))
;;   (exchange-point-and-mark)
;;   (phi-rectangle-set-mark-command)
;;   (exchange-point-and-mark))

(define-key my-map (kbd "C-x r Y") 'nispio/yank-rectangle-from-kill-ring)
(define-key my-map (kbd "C-x r D") 'delete-whitespace-rectangle)
(define-key my-map (kbd "C-c C-S-SPC") 'nispio/region-to-phi-rectangle)

(provide 'nispio/rect-utils)
