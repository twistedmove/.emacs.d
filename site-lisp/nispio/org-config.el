
;; Where are my Org files typically located? Org acutally uses this variable
;; only under rare circumstances, like when filing remember notes in an
;; interactive way and prompting you for an Org file to put the note into.
(setq org-directory "~/.org")

;; The list of agenda files is stored and maintained in the given file, one
;; agenda file per line.  Within this file, paths can be given relative to
;; `org-directory'.
(setq org-agenda-files "~/.org/agendas.ini")

;; When visiting an Org file, in what folding state do I first want to see it?
;; Many use #+STARTUP options to set this on a per-file basis.
(setq org-startup-folded t)

;; ;; When archiving an entry, where will it go?
;; ;; TODO: Learn about archiving
;; (setq org-archive-location "%s_archive::")

;; Make the outline more list-like by hiding all leading stars but one. 
(setq org-hide-leading-stars t)

;; Should C-a and C-e behave specially, considering the headline and not the
;; leading stars, todo keywords, or the trailing tags?
(setq org-special-ctrl-a/e t)

;; Should ido.el be used for completion whenever it makes sense? 
(setq org-completion-use-ido t)
(setq org-outline-path-complete-in-steps nil)

;; Should pressing RET on a hyperlink follow the link?
(setq org-return-follows-link t)

;; Org-mode tries to be smart about inserting blank lines before new
;; entries/items, by looking at what is before the previous
;; entry/item. Customize this to out-smart it.
(setq org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))

;; Which TODO keywords should be used? A "!" means that a timestamp should be
;; added when changing to/from this state.  A "@" means that a note is required
;; when changing to/from this state.
(setq org-todo-keywords '((sequence
                           "TODO(t)"
                           "STARTED(s!)"
                           "WAIT(w@/!)"
                           "|"
                           "DONE(d)"
                           "COMPLETE(c!)")))

;; Should unfinished children block state changes in the parent? 
(setq org-enforce-todo-dependencies t)

;; Should unfinished checkboxes block state changes in the parent?
;; TODO: find out what checkboxes are
(setq org-enforce-todo-checkbox-dependencies t)

;; ;; Which tags should be available? Note that tags besides the configured ones
;; ;; can be used, but for the important ones you can define keys for fast access
;; ;; here.
(setq org-tag-alist '(("CODE" . ?c)
                      ("PLAN" . ?p)
                      ("EXPERIMENT" . ?e)
                      ("LEARN" . ?l)
                      ("DOCUMENT" . ?d)
                      ("TEACH" . ?t)))

;; Make the tags interface even faster for changing a single tag.
(setq org-fast-tag-selection-single-key 'expert)

;; How should tags be aligned in the headline? If it is negative, it means that
;; the tags should be flushright to that column.
(setq org-tags-column -77)

;; ;; When adding new entries (or tasks) to a list, do I want the entry to be first
;; ;; or last in the list?
;; ;; TODO: find out how this works
;; (setq org-reverse-note-order t)

;; ;; org-capture is great for fast capture of ideas, notes, and tasks. It is one
;; ;; of the primary capture methods in Org-mode.
;; ;; TODO: Find out about org-capture

;; ;; Prepare templates for the typical notes and tasks you want to capture
;; ;; quickly. I believe everyone using org-capture customizes this.
;; (setq org-capture-templates t)
 (setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/.org/tasks.org" "Unfiled Tasks")
		     "* TODO %?\n  - Added: %U")
		("T" "Todo (from file)" entry (file+headline "~/.org/tasks.org" "Linked Tasks")
             "* TODO %i%?\n  - Added: %U\n  - Link: %a")
        ("j" "Journal" entry (file+datetree "~/.org/journal.org")
             "* %?\n  - Added %U\n")))

(setq org-link-search-must-match-exact-headline)
;; ;; If you do not set up templates with target files, at least tell Org where to
;; ;; put captured notes.
;; (setq org-default-notes-file t)

;; ;; Refiling means moving entries around, for example from a capturing location
;; ;; to the correct project.
;; ;; TODO: learn about refiling

;; ;; What should be on the menu when you refile tasks with C-c C-w? 
;; (setq org-refile-targets t)

;; ;; How would you like to select refile targets. Headline only, or the path along
;; ;; the outline hierarchy?
;; (setq org-refile-use-outline-path t)

;; Should the agenda start on Monday, or better today? Non-nil means start the
;; overview always on the specified weekday.  0 denotes Sunday, 1 denotes
;; Monday, etc.  When nil, always start on the current day.
(setq org-agenda-start-on-weekday nil)

;; How many days should the default agenda show? Default is 7, a whole week. 
(setq org-agenda-ndays 10)

;; ;; Should the agenda also show entries from the Emacs diary?
;; ;; TODO: Find out about the emacs diary
;; (setq org-agenda-include-diary t)

;; ;; Define your own Agenda commands. Complex, advanced variable, but pretty much
;; ;; everyone ends up configuring it. Use customize to configure it, this is the
;; ;; best and safest way. Do checkout this tutorial on building your own custom
;; ;; agenda commands as well.
;; ;; TODO: find out about custom agenda commands
;; (setq org-agenda-custom-commands t)

;; ;; How should things be sorted in the agenda display?
;; ;; TODO: Learn more about agenda sorting
;; (setq org-agenda-sorting-strategy t)

;; To reduce clutter in the task list for today, many users like to remove tasks
;; from the daily list right when they are done. The following variables give
;; detailed control to what kind of entries this should apply:

;; Skip scheduled entries in agenda view if done
(setq org-agenda-skip-scheduled-if-done t)

;; Skip deadline entries in agenda view if done
(setq org-agenda-skip-deadline-if-done t)

;; People who use Org like a day planner, who schedule all tasks to specific
;; dates, often like to not have scheduled tasks listed in their global TODO
;; list, because scheduling it already means to have taking care of it in a
;; sense, and because they know they will run into these tasks in the agenda
;; anyway.

;; ;; Don't show deadline tasks in global TODO list. 
;; (setq org-agenda-todo-ignore-deadlines t)

;; ;; Don't show any tasks with a date in the global TODO list. 
;; (setq org-agenda-todo-ignore-with-date t)

;; ;; Don't show scheduled tasks in the global TODO list. 
;; (setq org-agenda-todo-ignore-scheduled t)

;; Should LaTeX fragments be converted to inline images for HTML output? 
(setq org-export-with-LaTeX-fragments t)

(provide 'nispio/org-config)
