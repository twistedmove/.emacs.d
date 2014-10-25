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

(defun nispio/show-prefix-arg (&optional arg)
  (interactive "P")
  (if (/= 0 (safe-length arg))
      (message "Prefix: %s (%d)" arg (safe-length arg))
    (message "Argument: %s (%d)" arg (safe-length arg))))

(defun nispio/trim-string (arg) 
  "Simple function for trimming the whitespace from the ends of
 a string. Also removes any string properties such as font faces."
  (let ((str (substring-no-properties arg)))
	(when (string-match "^[ \t]+" str)
	  (setq str (replace-match "" nil nil str)))
	(when (string-match "[ \t]+$" str)
	  (setq str (replace-match "" nil nil str)))
	str))

(defun nispio/directory-subdirs (directory &optional reject)
  "Returns a list of all subdirectories contained in DIRECTORY.
Optional argument REJECT can specify a list of subdirectory names
to ignore."
  (let* ((dir (file-name-as-directory directory))
		 (ls (directory-files (file-name-as-directory dir)))
		 (files (mapcar (lambda (el) (concat dir el)) ls))
		 (reject (append reject '("." "..")))
		 (prunes (mapcar (lambda (el) (concat dir el)) reject)))
	(prune-directory-list files nil prunes)))

(provide 'nispio/misc-utils)
