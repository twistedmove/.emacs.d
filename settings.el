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
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(diredp-hide-details-initially-flag nil)
 '(fci-rule-color "#14151E" t)
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
	((sequence "TODO(t!)" "STARTED(s!)" "WAIT(w@/!)" "ASK(a)" "|" "ANSWERED(A@)" "CANCELLED(x@)" "DONE(d)" "COMPLETE(c!)"))))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
	((20 . "#d54e53")
	 (40 . "goldenrod")
	 (60 . "#e7c547")
	 (80 . "DarkOliveGreen3")
	 (100 . "#70c0b1")
	 (120 . "DeepSkyBlue1")
	 (140 . "#c397d8")
	 (160 . "#d54e53")
	 (180 . "goldenrod")
	 (200 . "#e7c547")
	 (220 . "DarkOliveGreen3")
	 (240 . "#70c0b1")
	 (260 . "DeepSkyBlue1")
	 (280 . "#c397d8")
	 (300 . "#d54e53")
	 (320 . "goldenrod")
	 (340 . "#e7c547")
	 (360 . "DarkOliveGreen3"))))
 '(vc-annotate-very-old-color nil))
(put 'dired-find-alternate-file 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
