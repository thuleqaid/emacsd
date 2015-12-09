;; set coding priority order (later one with higher priority)
(prefer-coding-system 'gb2312)
(prefer-coding-system 'sjis)

;; backup setting
(if (not (file-accessible-directory-p "~/.emacs.d/backup"))
    (make-directory "~/.emacs.d/backup")
)
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))
(setq-default make-backup-file t)
(setq make-backup-file t)
(setq make-backup-files t)
(setq version-control t)
(setq kept-old-versions 2)
(setq kept-new-versions 10)
(setq delete-old-versions t)

;; time display
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time)

;; tab/whitespace/line number
(global-linum-mode 1)
(setq default-tab-width 4)
(setq tab-width 4)
(setq tab-stop-list '(4  8  12 16 20 24 28 32 36 40
                      44 48 52 56 60 64 68 72 76 80
                      84 88 92 96))
(setq whitespace-style '(face tabs space-mark))
(global-whitespace-mode t)

;;(global-ede-mode 1)
;;(require 'semantic/ia)
;;(setq semantic-default-submodes
;;      '(global-semantic-idle-scheduler-mode
;;        global-semanticdb-minor-mode
;;        global-semantic-idle-summary-mode
;;        global-semantic-idle-completions-mode
;;        global-semantic-highlight-func-mode
;;        global-semantic-decoration-mode
;;        global-semantic-mru-bookmark-mode))
;;(semantic-mode 1)

(require-package 'sr-speedbar)
(global-set-key (kbd "<f9>") (lambda()
                               (interactive)
                               (sr-speedbar-toggle)
                               (sr-speedbar-refresh)
                               (unless (sr-speedbar-exist-p)
                                 (kill-buffer "*SPEEDBAR*"))))
(require-package 'yasnippet)
(yas-global-mode 1)

(provide 'init-local)
