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
  "Show the raw prefix arg"
  (interactive "P")
  (message "%s" arg))

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


(defun nispio/set-window-size (window length &optional horizontal ignore)
  "Resize WINDOW vertically to length lines.
WINDOW can be an arbitrary window and defaults to the selected
one.  An attempt to resize the root window of a frame will raise
an error though.

For more information, refer to the doc string of
`window-resize'.
"
  (interactive (list nil (prefix-numeric-value current-prefix-arg) nil nil))
  (setq window (window-normalize-window window))
  (let* ((cur-height (window-body-height window))
		(cur-width (window-body-width window))
		(delta (if horizontal
				   (- length cur-width)
				 (- length cur-height))))
	(window-resize window delta horizontal ignore)))


(defun nispio/delete-window-maybe (&optional window)
  "Delete WINDOW.
WINDOW must be a valid window and defaults to the selected one.
Return nil.

If WINDOW is the root window of its frame, then make sure the
window is not dedicated to its buffer.

For more information see `delete-window'.
"
  (setq window (window-normalize-window window))
  (if (frame-root-window-p window)
	  (set-window-dedicated-p window nil)
	(delete-window window)))

(defun nispio/menu-item-property (menu item property value)
  "Set VALUE of named PROPERTY in menu item ITEM from MENU"
  (let* ((map (assoc item (cdr menu)))
		 (n (position property map)))
	(if (numberp n)
		 (setf (elt map (+ 1 n)) value)
	   (nconc map (list property value)))
	 map))

(provide 'nispio/misc-utils)
