;; Configure UI features
(scroll-bar-mode 0)			; Disable scroll bars
(tooltip-mode 1)			; Enable tooltips
(fringe-mode '(nil . 0))	; Left fringes only
(tool-bar-mode 0)			; Disable toolbar

;; Fromat the appearance of the mode line
(setq-default mode-line-format
 '("%e"
   mode-line-front-space
   mode-line-mule-info
   mode-line-client
   mode-line-modified
   mode-line-remote
   mode-line-frame-identification
   mode-line-buffer-identification
   " %3l :%3c  "
   mode-line-modes
   " "
   (vc-mode vc-mode)
   (global-mode-string global-mode-string)
   mode-line-end-spaces))

(load-theme 'nispio-dark t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-safe-themes
   (quote
	("efe9aa1a078bf00a43342a1fc8b16505d444f8068285f5f83c6275cadcc44b7d" "9b402e9e8f62024b2e7f516465b63a4927028a7055392290600b776e4a5b9905" "94ba29363bfb7e06105f68d72b268f85981f7fba2ddef89331660033101eb5e5" "12722541c8998f056b761bf63a92216aaf4610e4eb1afe7991842a31fa28b6d8" "90d329edc17c6f4e43dbc67709067ccd6c0a3caa355f305de2041755986548f2" default)))
 '(diredp-hide-details-initially-flag nil)
 '(org-agenda-ndays 10 t)
 '(org-agenda-restore-windows-after-quit t t)
 '(org-agenda-skip-deadline-if-done nil t)
 '(org-agenda-skip-scheduled-if-done nil t)
 '(org-agenda-start-on-weekday nil t)
 '(org-agenda-todo-ignore-scheduled t t)
 '(org-blank-before-new-entry (quote ((heading) (plain-list-item))))
 '(org-capture-templates
   (quote
	(("t" "Todo" entry
	  (file+headline "~/.org/tasks.org" "Unfiled Tasks")
	  "* TODO %i%?
  - State \"TODO\"       from \"\"           %U")
	 ("T" "Todo (from file)" entry
	  (file+headline "~/.org/tasks.org" "Unfiled Tasks")
	  (function nispio/linked-todo)
	  :immediate-finish t)
	 ("d" "Distraction" entry
	  (file+headline "~/.org/distractions.org" "Distractions")
	  "* %?
  - Added: %U")
	 ("D" "Distraction (as TODO)" entry
	  (file+headline "~/.org/distractions.org" "Tasks")
	  "* TODO %?
  - Added: %U")
	 ("n" "Notes" entry
	  (file+headline "~/.org/notes.org" "Notes")
	  "* %i%?
  - Added: %U")
	 ("N" "Notes (from file)" entry
	  (file+headline "~/.org/notes.org" "Notes")
	  (function nispio/linked-note))
	 ("p" "Pomodoros" entry
	  (file+datetree "~/.org/pomodoros.org")
	  "* TODO %i%?
  - State \"TODO\"       from \"\"           %U" :jump-to-captured t)
	 ("j" "Journal" entry
	  (file+datetree "~/.org/journal.org")
	  "* %?")
	 ("J" "Journal (free writing)" entry
	  (file+datetree "~/.org/freejourn.org")
	  "* %?"))) t)
 '(org-completion-use-ido t)
 '(org-enforce-todo-checkbox-dependencies t)
 '(org-enforce-todo-dependencies t)
 '(org-export-with-LaTeX-fragments t t)
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-hide-leading-stars t)
 '(org-link-search-must-match-exact-headline nil)
 '(org-outline-path-complete-in-steps nil)
 '(org-return-follows-link t)
 '(org-special-ctrl-a/e t)
 '(org-startup-folded t)
 '(org-tag-alist
   (quote
	(("CODE" . 99)
	 ("PLAN" . 112)
	 ("EXPERIMENT" . 101)
	 ("LEARN" . 108)
	 ("DOCUMENT" . 100)
	 ("TEACH" . 116))))
 '(org-tags-column -77)
 '(org-todo-keywords
   (quote
	((sequence "TODO(t!)" "STARTED(s!)" "WAIT(w@/!)" "ASK(a)" "|" "ANSWERED(A@)" "CANCELLED(x@)" "DONE(d)" "COMPLETE(c!)")))))
(put 'dired-find-alternate-file 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
