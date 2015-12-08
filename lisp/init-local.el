;;(setq indent-tabs-mode 1)
;;(setq default-tab-width 4)
;;(setq tab-width 4)
;;(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40
;;      44 48 52 56 60 64 68 72 76 80 84 88 92 96))
;;(global-whitespace-mode 1)
;;
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

(global-linum-mode 1)
(require-package 'sr-speedbar)
(global-set-key (kbd "<f9>") (lambda()
                               (interactive)
                               (sr-speedbar-toggle)
                               (sr-speedbar-refresh)
                               (unless (sr-speedbar-exist-p)
                                 (kill-buffer "*SPEEDBAR*"))))
(require-package 'yasnippet)

(provide 'init-local)
