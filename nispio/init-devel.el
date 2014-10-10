;; Set up indenting in C/C++
(setq c-default-style "linux")
(setq-default c-basic-offset 4)
(setq-default tab-width 4)
(c-set-offset 'inline-open 0)

;; Set up C-mode specific keybindings
(defun nispio/c-mode-keys-hook ()
  (local-set-key (kbd "C-c C-c") 'nispio/compile-c)
  (local-set-key (kbd "<f5>") 'nispio/run-debugger)
  (local-set-key (kbd "<S-f5>") 'nispio/debug-other-frame))
(add-hook 'c-mode-common-hook 'nispio/c-mode-keys-hook)

;; Shortcut for opening and closing braces in c-mode
(defun nispio/insert-braces ()
  (interactive)
  (execute-kbd-macro '[return 123 tab return return 125 tab 16 tab]))
(defun nispio/insert-braces-hook ()
  (local-set-key (kbd "<C-m>") 'nispio/insert-braces))
(add-hook 'c-mode-common-hook 'nispio/insert-braces-hook)

;; Make C mode use C++-style commenting
(add-hook 'c-mode-hook (lambda () (setq comment-start "// " comment-end  "")))

