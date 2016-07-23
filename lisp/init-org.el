(when (< emacs-major-version 24)
  (require-package 'org))
(require-package 'org-fstree)
;;;(when *is-a-mac*
;;;  (require-package 'org-mac-link)
;;;  (autoload 'org-mac-grab-link "org-mac-link" nil t)
;;;  (require-package 'org-mac-iCal))

(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)

;; Various preferences
(setq org-log-done t
      org-completion-use-ido t
      org-edit-timestamp-down-means-later t
      org-archive-mark-done nil
      org-catch-invisible-edits 'show
      org-export-coding-system 'utf-8
      org-fast-tag-selection-single-key 'expert
      org-html-validation-link nil
      org-export-kill-product-buffer-when-displayed t
      org-tags-column 80)


;; Lots of stuff from http://doc.norang.ca/org-mode.html

(defun sanityinc/grab-ditaa (url jar-name)
  "Download URL and extract JAR-NAME as `org-ditaa-jar-path'."
  ;; TODO: handle errors
  (message "Grabbing " jar-name " for org.")
  (let ((zip-temp (make-temp-name "emacs-ditaa")))
    (unwind-protect
        (progn
          (when (executable-find "unzip")
            (url-copy-file url zip-temp)
            (shell-command (concat "unzip -p " (shell-quote-argument zip-temp)
                                   " " (shell-quote-argument jar-name) " > "
                                   (shell-quote-argument org-ditaa-jar-path)))))
      (when (file-exists-p zip-temp)
        (delete-file zip-temp)))))

(defun sanityinc/grab-plantuml (url)
  "Download URL as `org-plantuml-jar-path'."
  (unwind-protect
      (progn
        (url-copy-file url (shell-quote-argument org-plantuml-jar-path)))))

(after-load 'ob-ditaa
  (unless (file-exists-p org-ditaa-jar-path)
    (let ((jar-name "ditaa0_9.jar")
          (url "http://jaist.dl.sourceforge.net/project/ditaa/ditaa/0.9/ditaa0_9.zip"))
      (setq org-ditaa-jar-path (expand-file-name jar-name (file-name-directory user-init-file)))
      (unless (file-exists-p org-ditaa-jar-path)
        (sanityinc/grab-ditaa url jar-name))))
  )
(after-load 'ob-plantuml
  (let ((jar-name "plantuml.jar")
        (url "http://jaist.dl.sourceforge.net/project/plantuml/plantuml.jar"))
    (setq org-plantuml-jar-path (expand-file-name jar-name (file-name-directory user-init-file)))
    (unless (file-exists-p org-plantuml-jar-path)
      (sanityinc/grab-plantuml url)))
  )


(define-minor-mode prose-mode
  "Set up a buffer for prose editing.
This enables or modifies a number of settings so that the
experience of editing prose is a little more like that of a
typical word processor."
  nil " Prose" nil
  (if prose-mode
      (progn
        (setq truncate-lines nil)
        (setq word-wrap t)
        (setq cursor-type 'bar)
        (when (eq major-mode 'org)
          (kill-local-variable 'buffer-face-mode-face))
        (buffer-face-mode 1)
        ;;(delete-selection-mode 1)
        (set (make-local-variable 'blink-cursor-interval) 0.6)
        (set (make-local-variable 'show-trailing-whitespace) nil)
        (flyspell-mode 1)
        (when (fboundp 'visual-line-mode)
          (visual-line-mode 1)))
    (kill-local-variable 'truncate-lines)
    (kill-local-variable 'word-wrap)
    (kill-local-variable 'cursor-type)
    (kill-local-variable 'show-trailing-whitespace)
    (buffer-face-mode -1)
    ;; (delete-selection-mode -1)
    (flyspell-mode -1)
    (when (fboundp 'visual-line-mode)
      (visual-line-mode -1))))

;;(add-hook 'org-mode-hook 'buffer-face-mode)


(setq org-support-shift-select t)

;;; Capturing

(global-set-key (kbd "C-c c") 'org-capture)

(setq org-capture-templates
      `(("t" "todo" entry (file "")  ; "" => org-default-notes-file
         "* NEXT %?\n%U\n" :clock-resume t)
        ("n" "note" entry (file "")
         "* %? :NOTE:\n%U\n%a\n" :clock-resume t)
        ))



;;; Refiling

(setq org-refile-use-cache nil)

; Targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))

(after-load 'org-agenda
  (add-to-list 'org-agenda-after-show-hook 'org-show-entry))

;; Exclude DONE state tasks from refile targets
(defun sanityinc/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets."
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
(setq org-refile-target-verify-function 'sanityinc/verify-refile-target)

(defun sanityinc/org-refile-anywhere (&optional goto default-buffer rfloc msg)
  "A version of `org-refile' which suppresses `org-refile-target-verify-function'."
  (interactive "P")
  (let ((org-refile-target-verify-function))
    (org-refile goto default-buffer rfloc msg)))

;; Targets start with the file name - allows creating level 1 tasks
;;(setq org-refile-use-outline-path (quote file))
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)

;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes 'confirm)


;;; To-do settings

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
              (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
              (sequence "WAITING(w@/!)" "HOLD(h)" "|" "CANCELLED(c@/!)"))))

(setq org-todo-keyword-faces
      (quote (("NEXT" :inherit warning)
              ("PROJECT" :inherit font-lock-string-face))))



;;; Agenda views

(setq-default org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3))


(let ((active-project-match "-INBOX/PROJECT"))

  (setq org-stuck-projects
        `(,active-project-match ("NEXT")))

  (setq org-agenda-compact-blocks t
        org-agenda-sticky t
        org-agenda-start-on-weekday nil
        org-agenda-span 'day
        org-agenda-include-diary nil
        org-agenda-sorting-strategy
        '((agenda habit-down time-up user-defined-up effort-up category-keep)
          (todo category-up effort-up)
          (tags category-up effort-up)
          (search category-up))
        org-agenda-window-setup 'current-window
        org-agenda-custom-commands
        `(("N" "Notes" tags "NOTE"
           ((org-agenda-overriding-header "Notes")
            (org-tags-match-list-sublevels t)))
          ("g" "GTD"
           ((agenda "" nil)
            (tags "INBOX"
                  ((org-agenda-overriding-header "Inbox")
                   (org-tags-match-list-sublevels nil)))
            ;; (stuck ""
            ;;        ((org-agenda-overriding-header "Stuck Projects")
            ;;         (org-agenda-tags-todo-honor-ignore-options t)
            ;;         (org-tags-match-list-sublevels t)
            ;;         (org-agenda-todo-ignore-scheduled 'future)))
            (tags-todo "-INBOX/NEXT"
                       ((org-agenda-overriding-header "Next Actions")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        ;; TODO: skip if a parent is WAITING or HOLD
                        (org-tags-match-list-sublevels t)
                        (org-agenda-sorting-strategy
                         '(todo-state-down effort-up category-keep))))
            (tags-todo ,active-project-match
                       ((org-agenda-overriding-header "Projects")
                        (org-tags-match-list-sublevels t)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "-INBOX/-NEXT"
                       ((org-agenda-overriding-header "Orphaned Tasks")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        ;; TODO: skip if a parent is a project
                        (org-agenda-skip-function
                         '(lambda ()
                            (or (org-agenda-skip-subtree-if 'todo '("PROJECT" "HOLD" "WAITING"))
                                (org-agenda-skip-subtree-if 'nottododo '("TODO")))))
                        (org-tags-match-list-sublevels t)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "/WAITING"
                       ((org-agenda-overriding-header "Waiting")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "-INBOX/HOLD"
                       ((org-agenda-overriding-header "On Hold")
                        ;; TODO: skip if a parent is WAITING or HOLD
                        (org-tags-match-list-sublevels nil)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            ;; (tags-todo "-NEXT"
            ;;            ((org-agenda-overriding-header "All other TODOs")
            ;;             (org-match-list-sublevels t)))
            )))))


(add-hook 'org-agenda-mode-hook 'hl-line-mode)


;;; Org clock

;; Save the running clock and all clock history when exiting Emacs, load it on startup
(after-load 'org
  (org-clock-persistence-insinuate))
(setq org-clock-persist t)
(setq org-clock-in-resume t)

;; Save clock data and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Save state changes in the LOGBOOK drawer
(setq org-log-into-drawer t)
;; Removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; Show clock sums as hours and minutes, not "n days" etc.
(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))



;;; Show the clocked-in task - if any - in the header line
(defun sanityinc/show-org-clock-in-header-line ()
  (setq-default header-line-format '((" " org-mode-line-string " "))))

(defun sanityinc/hide-org-clock-from-header-line ()
  (setq-default header-line-format nil))

(add-hook 'org-clock-in-hook 'sanityinc/show-org-clock-in-header-line)
(add-hook 'org-clock-out-hook 'sanityinc/hide-org-clock-from-header-line)
(add-hook 'org-clock-cancel-hook 'sanityinc/hide-org-clock-from-header-line)

(after-load 'org-clock
  (define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
  (define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu))



;;;(when (and *is-a-mac* (file-directory-p "/Applications/org-clock-statusbar.app"))
;;;  (add-hook 'org-clock-in-hook
;;;            (lambda () (call-process "/usr/bin/osascript" nil 0 nil "-e"
;;;                                (concat "tell application \"org-clock-statusbar\" to clock in \"" org-clock-current-task "\""))))
;;;  (add-hook 'org-clock-out-hook
;;;            (lambda () (call-process "/usr/bin/osascript" nil 0 nil "-e"
;;;                                "tell application \"org-clock-statusbar\" to clock out"))))



;; Remove empty LOGBOOK drawers on clock out
(defun sanityinc/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at "LOGBOOK" (point))))

(after-load 'org-clock
  (add-hook 'org-clock-out-hook 'sanityinc/remove-empty-drawer-on-clock-out 'append))



;; TODO: warn about inconsistent items, e.g. TODO inside non-PROJECT
;; TODO: nested projects!



;;; Archiving

(setq org-archive-mark-done nil)
(setq org-archive-location "%s_archive::* Archive")





(require-package 'org-pomodoro)
(setq org-pomodoro-keep-killed-pomodoro-time t)
(after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro))


;; ;; Show iCal calendars in the org agenda
;; (when (and *is-a-mac* (require 'org-mac-iCal nil t))
;;   (setq org-agenda-include-diary t
;;         org-agenda-custom-commands
;;         '(("I" "Import diary from iCal" agenda ""
;;            ((org-agenda-mode-hook #'org-mac-iCal)))))

;;   (add-hook 'org-agenda-cleanup-fancy-diary-hook
;;             (lambda ()
;;               (goto-char (point-min))
;;               (save-excursion
;;                 (while (re-search-forward "^[a-z]" nil t)
;;                   (goto-char (match-beginning 0))
;;                   (insert "0:00-24:00 ")))
;;               (while (re-search-forward "^ [a-z]" nil t)
;;                 (goto-char (match-beginning 0))
;;                 (save-excursion
;;                   (re-search-backward "^[0-9]+:[0-9]+-[0-9]+:[0-9]+ " nil t))
;;                 (insert (match-string 0))))))


(after-load 'org
  (define-key org-mode-map (kbd "C-M-<up>") 'org-up-element)
;;;  (when *is-a-mac*
;;;    (define-key org-mode-map (kbd "M-h") nil)
;;;    (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link))
  )

(after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   `((ditaa . t)
     (dot . t)
     (emacs-lisp . t)
     (gnuplot . t)
     (haskell . nil)
     (latex . nil)
     (ledger . nil)
     (ocaml . nil)
     (octave . t)
     (plantuml . t)
     (python . t)
     (ruby . nil)
     (screen . nil)
     (,(if (locate-library "ob-sh") 'sh 'shell) . t)
     (sql . nil)
     (sqlite . t))))

;GTD Workflow
;1. capture and save in notes
;2. dispatch items in notes to todolist, droplist and reflist
;3. archive finished items in todolist
(let* (
       (agenda-path (expand-file-name "agenda/" user-emacs-directory))
       (agenda-file (expand-file-name "todolist.org" agenda-path))
       (agenda-file2 (expand-file-name "droplist.org" agenda-path))
       (agenda-file3 (expand-file-name "reflist.org" agenda-path))
       (note-file (expand-file-name "notes.org" agenda-path)))
  (setq org-agenda-files (list agenda-file note-file))
  (setq org-default-notes-file note-file)
  (setq diary-file (expand-file-name "diary" user-emacs-directory))
  (unless (file-exists-p agenda-path)
    (make-directory agenda-path))
  (unless (file-exists-p agenda-file)
    (append-to-file "# -*- mode:org; coding:utf-8 -*-\n" nil agenda-file)
    )
  (unless (file-exists-p agenda-file2)
    (append-to-file "# -*- mode:org; coding:utf-8 -*-\n" nil agenda-file2)
    )
  (unless (file-exists-p agenda-file3)
    (append-to-file "# -*- mode:org; coding:utf-8 -*-\n" nil agenda-file3)
    )
  (unless (file-exists-p diary-file)
    (append-to-file "# -*- coding:utf-8 -*-\n" nil diary-file)
    )
  (unless (file-exists-p note-file)
    (append-to-file "# -*- mode:org; coding:utf-8 -*-\n" nil note-file)
    )
  )
(setq org-agenda-include-diary t)
(org-agenda-to-appt)
(setq appt-display-format 'window)
(setq appt-display-duration 60)
(setq appt-audible t)
(setq appt-display-mode-line t)
(setq appt-message-warning-time 10)
(setq appt-display-diary t)
(setq calendar-date-style 'iso)
(add-hook 'calendar-today-visible-hook 'calendar-mark-today)
(add-hook 'diary-list-entries-hook 'diary-sort-entries t)
(after-load 'init (appt-activate 1))
;; (appt-activate 1)

(setq org-list-demote-modify-bullet
      '(("+" . "-") ("-" . "+") ("*" . "+")))
(defun my-org-confirm-babel-evaluate (lang body)
  (if (member lang '("plantuml" "gnuplot")) nil t)) ; don't ask for plantuml, gnuplot
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)
(setq org-use-sub-superscripts nil)
(setq org-export-with-sub-superscripts nil)

(defun thuleqaid/org-babel-image-format ()
  (interactive)
  (save-excursion
    (let ((image-format "png"))
      (setq image-format (read-from-minibuffer "Image Format(png/svg): ")) ; read target image format
      (goto-char (point-min)) ; replace gnuplot script for output mode
      (while (re-search-forward "^\\(\\s *set term \\)\\(\\S +\\) " nil t)
        (replace-match (format "\\1%s " image-format)))
      (goto-char (point-min)) ; replace outfile name
      (while (re-search-forward "^\\(\\s *#\\+begin_src \\(?:plantuml\\|gnuplot\\) :file \\S +\\.\\)\\(\\S +\\) " nil t)
        (replace-match (format "\\1%s " image-format)))
      )))
(defun thuleqaid/org-babel-batch-src ()
  (interactive)
  (save-excursion
    (goto-char (point-min)) ; replace outfile name
    (while (re-search-forward "^\\(\\s *#\\+begin_src \\(?:plantuml\\|gnuplot\\) :file \\S +\\.\\)\\(\\S +\\) " nil t)
      (org-babel-execute-src-block))
    ))

(require 'ox-reveal)
(setq org-reveal-root (format "file:///%s" (expand-file-name "reveal.js" user-emacs-directory)))

(provide 'init-org)
