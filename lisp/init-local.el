;; set coding priority order (later one with higher priority)
;(prefer-coding-system 'gb2312)
;(prefer-coding-system 'sjis)
; Chinese coding setting in Windons (copy and modify from init-locales.el)
;(set-language-environment 'chinese-GBK)
;(setq locale-coding-system 'gbk)
;(set-default-coding-systems 'gbk)
;(set-terminal-coding-system 'gbk)
;(set-selection-coding-system 'gbk)
(setq session-save-file-coding-system 'utf-8)

(autoload 'chinese-wbim-use-package "chinese-wbim" "Another emacs input method")
;; Tooltip 暂时还不好用
(setq chinese-wbim-use-tooltip nil)
(register-input-method
 "chinese-wbim" "euc-cn" 'chinese-wbim-use-package
 "五笔" "汉字五笔输入法" "wb.txt")
;; 用 ; 暂时输入英文
(require 'chinese-wbim-extra)
(global-set-key ";" 'chinese-wbim-insert-ascii)
;设置默认输入法
(setq default-input-method 'chinese-wbim)

(auto-image-file-mode t)
(iimage-mode t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((plantuml . t)))
(setq org-plantuml-jar-path
      (expand-file-name "~/.emacs.d/plantuml.jar"))

(require 'calendar-fate)
(calendar-fate-chinese-character)
(setq holiday-other-holidays '((holiday-chinese-terms)))

(setq calendar-date-style 'iso)
;(setq org-time-stamp-custom-formats '("<%Y/%m/%d>" . "<%Y/%m/%d %H:%M>"))
(setq appt-display-format 'window)
(setq appt-display-duration 60)
(setq appt-audible t)
(setq appt-display-mode-line t)
(setq appt-message-warning-time 10)
(setq appt-display-diary t)
(add-hook 'calendar-today-visible-hook 'calendar-mark-today)
(add-hook 'diary-list-entries-hook 'diary-sort-entries t)
(appt-activate 1)

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
(defun indent-tabs-mode-setup ()
  (setq indent-tabs-mode t))
(add-hook 'c-mode-hook 'indent-tabs-mode-setup)

(setq whitespace-style '(face tabs space-mark))
(global-whitespace-mode t)

(global-set-key (kbd "<f12>") 'highlight-symbol)
(global-set-key (kbd "M-<f12>") 'highlight-symbol-remove-all)

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
                               (if (sr-speedbar-exist-p)
                                 (sr-speedbar-select-window)
                                 (kill-buffer "*SPEEDBAR*"))))
(require-package 'yasnippet)
(yas-global-mode 1)

(ac-config-default)
(add-hook 'python-mode-hook 'ac-cc-mode-setup)

(require-package 'racer)
(require-package 'company)
(require-package 'company-racer)
(require-package 'flycheck)
(require-package 'flycheck-rust)
(require-package 'rust-mode)
(setq company-idle-delay 0.2)
(setq company-minimum-prefix-length 1)
(setq racer-cmd "/usr/local/bin/racer")
(setq racer-rust-src-path "/home/tinder/work/rust/rust/src/")
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(setq company-tooltip-align-annotations t)
(add-hook 'racer-mode-hook
          '(lambda ()
             (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
             (set (make-local-variable 'company-backends) '(company-racer))
             (local-set-key (kbd "TAB") #'racer-complete-or-indent)))

(provide 'init-local)
