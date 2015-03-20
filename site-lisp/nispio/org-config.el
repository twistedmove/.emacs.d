(require 'org)

;; When visiting an Org file, in what folding state do I first want to see it?
;; Many use #+STARTUP options to set this on a per-file basis.
(customize-set-variable 'org-startup-folded t)

;; ;; When archiving an entry, where will it go?
;; ;; TODO Learn about archiving
;; (customize-set-variable 'org-archive-location "%s_archive::")

;; Make the outline more list-like by hiding all leading stars but one. 
(customize-set-variable 'org-hide-leading-stars t)

;; Should C-a and C-e behave specially, considering the headline and not the
;; leading stars, todo keywords, or the trailing tags?
(customize-set-variable 'org-special-ctrl-a/e t)

;; Should ido.el be used for completion whenever it makes sense? 
(customize-set-variable 'org-completion-use-ido t)
(customize-set-variable 'org-outline-path-complete-in-steps nil)

;; Should pressing RET on a hyperlink follow the link?
(customize-set-variable 'org-return-follows-link t)

;; Org-mode tries to be smart about inserting blank lines before new
;; entries/items, by looking at what is before the previous
;; entry/item. Customize this to out-smart it.
(customize-set-variable 'org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))

;; Which TODO keywords should be used? A "!" means that a timestamp should be
;; added when changing to/from this state.  A "@" means that a note is required
;; when changing to/from this state.
(customize-set-variable 'org-todo-keywords '((sequence
                           "TODO(t!)"
                           "STARTED(s!)"
                           "WAIT(w@/!)"
                           "|"
                           "CANCELLED(x@)"
                           "DONE(d)"
                           "COMPLETE(c!)")))

;; Should unfinished children block state changes in the parent? 
(customize-set-variable 'org-enforce-todo-dependencies t)

;; Should unfinished checkboxes block state changes in the parent?
;; TODO find out what checkboxes are
(customize-set-variable 'org-enforce-todo-checkbox-dependencies t)

;; ;; Which tags should be available? Note that tags besides the configured ones
;; ;; can be used, but for the important ones you can define keys for fast access
;; ;; here.
(customize-set-variable 'org-tag-alist '(("CODE" . ?c)
                      ("PLAN" . ?p)
                      ("EXPERIMENT" . ?e)
                      ("LEARN" . ?l)
                      ("DOCUMENT" . ?d)
                      ("TEACH" . ?t)))

;; Make the tags interface even faster for changing a single tag.
(customize-set-variable 'org-fast-tag-selection-single-key 'expert)

;; How should tags be aligned in the headline? If it is negative, it means that
;; the tags should be flushright to that column.
(customize-set-variable 'org-tags-column -77)

;; ;; When adding new entries (or tasks) to a list, do I want the entry to be first
;; ;; or last in the list?
;; ;; TODO: find out how this works
;; (customize-set-variable 'org-reverse-note-order t)

;; ;; org-capture is great for fast capture of ideas, notes, and tasks. It is one
;; ;; of the primary capture methods in Org-mode.
;; ;; TODO: Find out about org-capture

;; ;; Prepare templates for the typical notes and tasks you want to capture
;; ;; quickly. I believe everyone using org-capture customizes this.
;; (customize-set-variable 'org-capture-templates t)
(customize-set-variable 'org-capture-templates
      '(("t" "Todo" entry (file+headline "~/.org/tasks.org" "Unfiled Tasks")
		     "* TODO %i%?\n  - State \"TODO\"       from \"\"           %U")
		("T" "Todo (from file)" entry (file+headline "~/.org/tasks.org" "Unfiled Tasks")
		     (function nispio/linked-todo)
		     :immediate-finish t)
		("d" "Distraction" entry (file+headline "~/.org/distractions.org" "Distractions")
		     "* %?\n  - Added: %U")
		("D" "Distraction (as TODO)" entry (file+headline "~/.org/distractions.org" "Tasks")
		     "* TODO %?\n  - Added: %U")
		("n" "Notes" entry (file+headline "~/.org/notes.org" "Notes")
		     "* %i%?\n  - Added: %U")
		("N" "Notes (from file)" entry (file+headline "~/.org/notes.org" "Notes")
		     (function nispio/linked-note))
		("p" "Pomodoros" entry (file+datetree "~/.org/pomodoros.org")
		     "* TODO %i%?\n  - State \"TODO\"       from \"\"           %U"
		     :jump-to-captured t)
        ("j" "Journal" entry (file+datetree "~/.org/journal.org")
             "* %?\n  - Added %U\n")))

(defun nispio/linked-todo ()
  "Return a template to be used for pomodoros"
  (interactive)
  (let* ((buffer (org-capture-get :buffer))
		 (file (buffer-file-name (or (buffer-base-buffer buffer) buffer)))
		 (filename (and (buffer-file-name buffer)
						(file-name-nondirectory (buffer-file-name buffer))))
		 initial)
	(setq initial (or org-capture-initial
					  (and (org-region-active-p)
						   (buffer-substring (point) (mark)))))
	(when (stringp initial)
	  (remove-text-properties 0 (length initial) '(read-only t) initial))
	(format "* %s\n  - State \"TODO\"       from \"\"           %s\n  - Link: %s"
			  "%i"                                             "%U"
			(org-make-link-string
			 (if initial (format "%s::%s" file initial) file)
			 filename))))

(defun nispio/linked-note ()
  "Return a template to be used for pomodoros"
  (interactive)
  (let* ((buffer (org-capture-get :buffer))
		 (file (buffer-file-name (or (buffer-base-buffer buffer) buffer)))
		 (filename (and (buffer-file-name buffer)
						(file-name-nondirectory (buffer-file-name buffer))))
		 (link (org-make-link-string file filename))
		 initial)
	(setq initial (or org-capture-initial
					  (and (org-region-active-p)
						   (buffer-substring (point) (mark)))))
	(when (stringp initial)
	  (remove-text-properties 0 (length initial) '(read-only t) initial))
	(format "* %s\n  - Added: %s\n  - Link: %s\n  %s"
			  "%?"           "%U"          link  "%i")))

;; Activate the org-timer module :
(add-to-list 'org-modules 'org-timer)

;; Set a default value for the timer, for example :
(setq org-timer-default-timer 25)

;; Modify the org-clock-in so that a timer is started with the default
;; value except if a timer is already started :
(add-hook 'org-clock-in-hook (lambda ()
			(if (not org-timer-current-timer) 
			  (org-timer-set-timer '(16)))))


(customize-set-variable 'org-link-search-must-match-exact-headline nil)
;; ;; If you do not set up templates with target files, at least tell Org where to
;; ;; put captured notes.
;; (customize-set-variable 'org-default-notes-file t)

;; Refiling means moving entries around, for example from a capturing location
;; to the correct project.
;; ;; TODO: learn about refiling

;; ;; What should be on the menu when you refile tasks with C-c C-w? 
;; (customize-set-variable 'org-refile-targets t)

;; ;; How would you like to select refile targets. Headline only, or the path along
;; ;; the outline hierarchy?
;; (customize-set-variable 'org-refile-use-outline-path t)

;; Should the agenda start on Monday, or better today? Non-nil means start the
;; overview always on the specified weekday.  0 denotes Sunday, 1 denotes
;; Monday, etc.  When nil, always start on the current day.
(customize-set-variable 'org-agenda-start-on-weekday nil)

;; How many days should the default agenda show? Default is 7, a whole week. 
(customize-set-variable 'org-agenda-ndays 10)

;; ;; Should the agenda also show entries from the Emacs diary?
;; ;; TODO: Find out about the emacs diary
;; (customize-set-variable 'org-agenda-include-diary t)

;; ;; Define your own Agenda commands. Complex, advanced variable, but pretty much
;; ;; everyone ends up configuring it. Use customize to configure it, this is the
;; ;; best and safest way. Do checkout this tutorial on building your own custom
;; ;; agenda commands as well.
;; ;; TODO: find out about custom agenda commands
;; (customize-set-variable 'org-agenda-custom-commands t)

;; ;; How should things be sorted in the agenda display?
;; ;; TODO Learn more about agenda sorting
;; (customize-set-variable 'org-agenda-sorting-strategy t)

;; To reduce clutter in the task list for today, many users like to remove tasks
;; from the daily list right when they are done. The following variables give
;; detailed control to what kind of entries this should apply:

;; Skip scheduled entries in agenda view if done
(customize-set-variable 'org-agenda-skip-scheduled-if-done t)

;; Skip deadline entries in agenda view if done
(customize-set-variable 'org-agenda-skip-deadline-if-done t)

;; People who use Org like a day planner, who schedule all tasks to specific
;; dates, often like to not have scheduled tasks listed in their global TODO
;; list, because scheduling it already means to have taking care of it in a
;; sense, and because they know they will run into these tasks in the agenda
;; anyway.

;; ;; Don't show deadline tasks in global TODO list. 
;; (customize-set-variable 'org-agenda-todo-ignore-deadlines t)

;; ;; Don't show any tasks with a date in the global TODO list. 
;; (customize-set-variable 'org-agenda-todo-ignore-with-date t)

;; ;; Don't show scheduled tasks in the global TODO list. 
;; (customize-set-variable 'org-agenda-todo-ignore-scheduled t)

;; Should LaTeX fragments be converted to inline images for HTML output? 
(customize-set-variable 'org-export-with-LaTeX-fragments t)

;; Restore window configuration upon exiting agenda. Before the window
;; configuration is changed for displaying the agenda,the current status is
;; recorded.  When the agenda is exited with `q' or `x' and this option is set,
;; the old state is restored.
(customize-set-variable 'org-agenda-restore-windows-after-quit t)


(provide 'nispio/org-config)
