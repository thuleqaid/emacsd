(when (< emacs-major-version 24)
  (require-package 'org))
(require-package 'org-fstree)
(require-package 'gnuplot)
(require-package 'htmlize)
(require 'calendar-fate)

;; agenda path
(setq org-agenda-path (expand-file-name "agenda/" user-emacs-directory))
;; dairy path
(setq org-diary-path (expand-file-name user-emacs-directory))
;; system encoding
(setq ffmpeg-cmd-encoding 'gbk)
;; ffmpeg -list_devices true -f dshow -i dummy 2>devices.txt
(setq ffmpeg-audio-device "マイク配列 (Realtek High Definition ")

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


;; download ditaa and plantuml
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
(let ((jar-name "plantuml.jar")
        (url "http://jaist.dl.sourceforge.net/project/plantuml/plantuml.jar"))
    (setq org-plantuml-jar-path (expand-file-name jar-name (file-name-directory user-init-file)))
    (unless (file-exists-p org-plantuml-jar-path)
      (sanityinc/grab-plantuml url)))


;;(add-hook 'org-mode-hook 'buffer-face-mode)


(setq org-support-shift-select t)

;;; Capturing

(global-set-key (kbd "C-c c") 'org-capture)

(setq org-capture-templates
      `(("t" "todo" entry (file "")  ; "" => org-default-notes-file
         "* TODO %?\n%U\n" :clock-resume t)
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


;;; Agenda views

(setq-default org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3))

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
          ;; (agenda "" ((org-agenda-span 7)
          ;;             (org-agenda-sorting-strategy
          ;;              (quote ((agenda time-up priority-down tag-up))))
          ;;             (org-deadline-warning-days 0)))
          (tags-todo "PROJECT"
                     ((org-agenda-overriding-header "Project")
                      (org-agenda-tags-todo-honor-ignore-options t)
                      (org-agenda-todo-ignore-scheduled 'future)
                      (org-agenda-sorting-strategy '(category-keep))))
          (tags-todo "HOME"
                     ((org-agenda-overriding-header "Home")
                      (org-agenda-tags-todo-honor-ignore-options t)
                      (org-agenda-todo-ignore-scheduled 'future)
                      (org-agenda-sorting-strategy '(category-keep))))
          (todo "APPT"
                     ((org-agenda-overriding-header "Appointment")
                      (org-agenda-tags-todo-honor-ignore-options t)
                      (org-agenda-todo-ignore-scheduled 'future)
                      (org-agenda-sorting-strategy '(category-keep))))
          (todo "WAITING"
                     ((org-agenda-overriding-header "Waiting")
                      (org-agenda-tags-todo-honor-ignore-options t)
                      (org-agenda-todo-ignore-scheduled 'future)
                      (org-agenda-sorting-strategy '(category-keep))))
          ))
        ))

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


(after-load 'org
  (define-key org-mode-map (kbd "C-M-<up>") 'org-up-element)
  )

(thuleqaid/coding-system 'org-babel-execute-src-block 'utf-8)
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
;2. dispatch items in notes into appropriate position in todolist
;3. archive finished items in gtdfile
(defun thuleqaid/new-agenda-file (filepath)
  (let ((tmpl "# -*- mode:org; coding:utf-8 -*-


* org-mode configuration
#+STARTUP: overview
#+STARTUP: hidestars
#+STARTUP: logdone
#+COLUMNS: %38ITEM(Details) %TAGS(Context) %7TODO(To Do) %5Effort(Time){:} %6CLOCKSUM{Total}
#+PROPERTY: Effort_ALL 0 0:10 0:20 0:30 1:00 2:00 3:00 4:00 8:00
#+TAGS: HOME(h) PROJECT(p)
#+SEQ_TODO: TODO(t) STARTED(s) WAITING(w) APPT(a) | DONE(d) CANCELLED(c) DEFERRED(f)
"
              ))
    (append-to-file tmpl nil filepath))
  )
(let ((agenda-file (expand-file-name "todolist.org" org-agenda-path))
      (note-file (expand-file-name "notes.org" org-agenda-path)))
  (setq org-agenda-files (list agenda-file note-file))
  (setq org-default-notes-file note-file)
  (setq diary-file (expand-file-name "diary" org-diary-path))
  (unless (file-exists-p org-agenda-path)
    (make-directory org-agenda-path))
  (unless (file-exists-p org-diary-path)
    (make-directory org-diary-path))
  (unless (file-exists-p agenda-file)
    ;; (append-to-file "# -*- mode:org; coding:utf-8 -*-\n" nil agenda-file)
    (thuleqaid/new-agenda-file agenda-file)
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
  (if (member lang '("plantuml" "gnuplot" "dot")) nil t)) ; don't ask for plantuml, gnuplot
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
      (while (re-search-forward "^\\(\\s *#\\+begin_src \\(?:plantuml\\|gnuplot\\|dot\\) :file \\S +\\.\\)\\(\\S +\\) " nil t)
        (replace-match (format "\\1%s " image-format)))
      )))
(defun thuleqaid/org-babel-batch-src ()
  (interactive)
  (save-excursion
    (goto-char (point-min)) ; replace outfile name
    (while (re-search-forward "^\\(\\s *#\\+begin_src \\(?:plantuml\\|gnuplot\\|dot\\) :file \\S +\\.\\)\\(\\S +\\) " nil t)
      (let* ((info (org-babel-get-src-block-info))
             (params (nth 2 info))
             (file (cdr (assq :file params)))
             (fullpath (expand-file-name file))
             (dirname (directory-file-name (file-name-directory fullpath))))
        (unless (file-accessible-directory-p dirname)
          (make-directory dirname))
        (org-babel-execute-src-block))
      )
    ))

(defun thuleqaid/org-insert-sub-file ()
  (interactive)
  (let ((curdir (file-name-directory (buffer-file-name)))
        (curdt (format-time-string "%Y%m%d%H%M%S"))
        (subdesc (read-string "Description: "))
        )
    (make-directory curdt curdir)
    (org-insert-link nil (format "file:%s.org" curdt) subdesc)
    ))
(defun thuleqaid/org-clear-sub-dir ()
  (interactive)
  (let* ((curdir (file-name-directory (buffer-file-name)))
         (filelist (directory-files curdir t))
         (dirlist '())
         (orglist '())
         (rmdirlist '())
         curfile
         )
    ;; sepearte dirs and org files
    (dolist (curfile filelist)
      (if (file-directory-p curfile)
          (unless (string-match "/\.\.\?$" curfile)
            (setq dirlist (cons curfile dirlist)))
        (when (string-match "\.org$" curfile)
          (setq orglist (cons curfile orglist))))
      )
    ;; find empty dir without same name org file
    (dolist (curfile dirlist)
      (unless (> (length (directory-files curfile t)) 2)
        (unless (member (format "%s.org" curfile) orglist)
          (setq rmdirlist (cons curfile rmdirlist))
          )
        )
      )
    ;; delete unused dirs
    (dolist (curfile rmdirlist)
      (delete-directory curfile)
      )
    ))
(setq thuleqaid/record-current-file "")
(setq thuleqaid/record-start-time (current-time))
(defun thuleqaid/record-start ()
  (interactive)
  (when (executable-find "ffmpeg")
    (let ((cmdfmt "ffmpeg -f dshow -i audio=\"%s\" %s")
          (resdir (file-name-sans-extension (buffer-file-name)))
          (docfmt "\n#+BEGIN_EXPORT html\n<audio src=\"%s\" controls=\"controls\"></audio>\n#+END_EXPORT\n")
          )
      (setq thuleqaid/record-start-time (current-time))
      (setq thuleqaid/record-current-file (format "%s/%s.ogg" resdir (format-time-string "%Y%m%d%H%M%S" thuleqaid/record-start-time)))
      (unless (file-exists-p resdir)
        (make-directory resdir))
      (let ((coding-system-for-read ffmpeg-cmd-encoding)
            (coding-system-for-write ffmpeg-cmd-encoding)
            )
        (async-shell-command (format cmdfmt ffmpeg-audio-device thuleqaid/record-current-file))
        (insert (format docfmt (file-relative-name thuleqaid/record-current-file (file-name-directory (buffer-file-name)))))
        )
      ))
  )
(defun thuleqaid/record-tag ()
  (interactive)
  (let* ((curtime (current-time))
         (diffsec (tramp-time-diff curtime thuleqaid/record-start-time))
         (outhour (ffloor (/ diffsec 60 60)))
         (outminute (ffloor (/ diffsec 60)))
         (outsecond (fround (mod diffsec 60)))
         )
    (insert (format " *_%02.0f:%02.0f:%02.0f_* " outhour outminute outsecond))
    )
  )

(require-package 'ox-reveal)
(require 'ox-reveal)
(setq org-reveal-root (format "file:///%s" (expand-file-name "reveal.js" user-emacs-directory)))

(require-package 'ox-rst)

;; Use local copy of MathJax, use CDN version when lack of local files
(when (file-exists-p (expand-file-name "MathJax" user-emacs-directory))
  ;URL for CDN version
  (require 'ox-html)
  (setq backup-mathjax-file (cadar org-html-mathjax-options))
  ; Local directory for MathJax
  (setq local-mathjax-root (format "file:///%s" (expand-file-name "MathJax" user-emacs-directory)))
  ; MathJax filename with options after filename
  (setq local-mathjax-file (substring backup-mathjax-file (string-match "MathJax\\.js" backup-mathjax-file)))
  ; JavaScript after loading local version of MathJax
  (setq local-mathjax-contrib (format "
<script>
if (window.MathJax) {
  document.write('<script type=\"text\/x-mathjax-config\">MathJax.Ajax.config.path[\"Contrib\"] = \"%s/contrib\";<\\\/script>');
  } else {
  document.write('<script src=\"%s\"><\\\/script>');
}
</script>
" local-mathjax-root backup-mathjax-file))
  ; Update MathJax URL
  (setq org-html-mathjax-options (cons (list 'path (format "%s/%s" local-mathjax-root local-mathjax-file)) (cdr org-html-mathjax-options)))
  ; Update HTML template for loading MathJax
  (setq org-html-mathjax-template (format "%s%s" org-html-mathjax-template local-mathjax-contrib))
)

(provide 'init-org)
