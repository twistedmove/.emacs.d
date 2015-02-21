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


;; This function is used in nispio/run-debugger
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

;; (source: https://github.com/magnars/.emacs.d/blob/master/defuns/lisp-defuns.el)
(defun nispio/eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
	  (prin1 (eval (read (current-kill 0)))
			 (current-buffer))
	(error (message "Invalid expression")
		   (insert (current-kill 0)))))

(defun nispio/calc-grab-number (top bot)
  "Parse the region as a single number and push it on the Calculator stack."
  (interactive "r")
  (require 'calc-ext)
  (calc-do-grab-region top bot '(4)))


;; (source: http://emacs.stackexchange.com/a/81/93)
(defun nispio/switch-to-scratch-and-back ()
  "Toggle between *scratch-MODE* buffer and the current buffer.
If a scratch buffer does not exist, create it with the major mode set to that
of the buffer from where this function is called."
  (interactive)
  (if (string-match "*scratch" (format "%s" (current-buffer)))
      (switch-to-buffer (other-buffer))
    (let ((mode-str (format "%s" major-mode)))
      (let ((scratch-buffer-name (get-buffer-create (concat "*scratch-" mode-str "*"))))
        (switch-to-buffer scratch-buffer-name)
        ; (source: http://stackoverflow.com/q/7539615)
        (funcall (intern mode-str))))))

;; (source: http://www.emacswiki.org/emacs/ReBuilder)
(defun nispio/reb-query-replace (to-string)
  "Replace current RE from point with `query-replace-regexp'."
  (interactive
   (progn (barf-if-buffer-read-only)
		  (list (query-replace-read-to
				 (reb-target-binding reb-regexp)
				 "Query replace"
				 t ))))
  (with-current-buffer reb-target-buffer
	(query-replace-regexp (reb-target-binding reb-regexp) to-string)))



(defvar find-exts "py m el"
  "Last arguments given to `find' by \\[nispio/dired-find-exts].")

(defvar find-exts-history nil)

(defun nispio/dired-find-exts (dir exts)
    "Search DIR recursively for files matching with extensions matching EXTS,
and run Dired on those files.
EXTS is a shell wildcard (not an Emacs regexp) and need not be quoted.
The default command run (after changing into DIR) is

    find . (-path */.git -prune -o (-name *.ext1 -o -name *.ext2)) -a -type f

This function is a wrapper around \\[find-dired]."
  (interactive (list (read-directory-name "Run find in directory: " nil "" t)
					 (read-string "Find extensions (no . or *): " find-exts
								  '(find-exts-history . 1))))
  (let* ((dir (file-name-as-directory (expand-file-name dir)))
		 (ext-list (split-string exts))
		 (ext-glob (lambda (x) (concat "\\*." x)))
		 (find-args (mapconcat ext-glob ext-list " -o -name "))
		 (find-args (concat
					 "\\( -path \\*/.git -prune -o \\( -name "
					 find-args
					 " \\) \\) -a -type f")))
	;; Store this time's input for next time
	(setq find-exts exts)
	;; Call `find-dired' to do the rest of the work
	(find-dired dir find-args)))

;; Open all marked files at once, but only show one
(defun nispio/find-marked-files ()
  (interactive)
  (let ((file (dired-get-filename nil t))
		buf)
	(dired-do-find-marked-files 0)
	(quit-window)
	(setq buf (and file (get-file-buffer file)))
	(and buf (show-buffer nil buf))))



(provide 'nispio/misc-utils)
