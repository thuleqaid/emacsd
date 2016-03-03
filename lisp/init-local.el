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

(setq sanityinc/force-default-font-for-symbols t)

(require-package 'chinese-wbim)
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

(require 'calendar-fate)
(calendar-fate-chinese-character)
(setq holiday-other-holidays '((holiday-chinese-terms)))

;; ;(setq org-time-stamp-custom-formats '("<%Y/%m/%d>" . "<%Y/%m/%d %H:%M>"))

(defun indent-tabs-mode-setup ()
  (setq indent-tabs-mode t))
(add-hook 'c-mode-hook 'indent-tabs-mode-setup)
(ac-config-default)
(add-hook 'python-mode-hook 'ac-cc-mode-setup)

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
(setq speedbar-show-unknown-files t)
(global-set-key (kbd "<f9>") (lambda()
                               (interactive)
                               (sr-speedbar-toggle)
                               (sr-speedbar-refresh)
                               (if (sr-speedbar-exist-p)
                                   (sr-speedbar-select-window)
                                 (kill-buffer "*SPEEDBAR*"))))

;;添加模版
(auto-insert-mode)  ;;; Adds hook to find-files-hook
(setq auto-insert-directory "~/.emacs.d/templates/")
(setq auto-insert-query nil)
(setq auto-insert-alist
      (append '(
                (org-mode . "template.org")
                (python-mode . "template.py")
                )
              auto-insert-alist))

(global-unset-key (kbd "M-c"))
(global-set-key (kbd "M-c c") 'capitalize-word)
(setq avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s))
(global-set-key (kbd "M-c f") 'avy-goto-char)
(global-unset-key (kbd "M-z"))
(global-unset-key (kbd "M-Z"))
(global-set-key (kbd "M-c z") 'zap-to-char)
(global-set-key (kbd "M-c Z") 'zap-up-to-char)
(global-set-key (kbd "M-c *") 'isearch-forward-symbol-at-point)
(defun thuleqaid/gotofile ()
  (interactive)
  (let* (
         (filename (ffap-file-exists-string (ffap-string-at-point)))
         (filenamelen (length filename))
         (jumpline 1))
    (if filename
        (save-excursion
          (if (< (point) filenamelen)
              (goto-char (point-min))
            (backward-char filenamelen))
          (nonincremental-re-search-forward (concat (regexp-quote filename) "\\s-+"))
          (setq jumpline (thing-at-point 'number))
          (ffap filename)
          (if jumpline (goto-line jumpline))
          ))
    ))
(global-set-key (kbd "M-c g f") 'ffap)
(global-set-key (kbd "M-c g F") 'thuleqaid/gotofile)

(setq tramp-mode nil)
(setq tramp-ssh-controlmaster-options "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")
(require-package 'helm)
(require 'helm-config)
;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-M-x-fuzzy-match                  t
      helm-buffers-fuzzy-match              t
      helm-recentf-fuzzy-match              t
      helm-semantic-fuzzy-match             t
      helm-imenu-fuzzy-match                t
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)
(require-package 'helm-gtags)
;; Enable helm-gtags-mode
(setq helm-gtags-ignore-case t
      helm-gtags-auto-update t
      helm-gtags-use-input-at-cursor t
      helm-gtags-pulse-at-cursor t
      helm-gtags-suggested-key-mapping t
      )
;; Enable helm-gtags-mode in Dired so you can jump to any tag
;; when navigate project tree with Dired
(add-hook 'dired-mode-hook 'helm-gtags-mode)
;; Enable helm-gtags-mode in Eshell for the same reason as above
(add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
;; Set key bindings
;; (eval-after-load "helm-gtags"
;;   '(progn
;;      (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
;;      (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
;;      (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
;;      (define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
;;      (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
;;      (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
;;      (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)))
(helm-mode 1)

;;;(require-package 'racer)
;;;(require-package 'company)
;;;(require-package 'company-racer)
;;;(require-package 'flycheck)
;;;(require-package 'flycheck-rust)
;;;(require-package 'rust-mode)
;;;(setq company-idle-delay 0.2)
;;;(setq company-minimum-prefix-length 1)
;;;(setq racer-cmd "/usr/local/bin/racer")
;;;(setq racer-rust-src-path "/home/tinder/work/rust/rust/src/")
;;;(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
;;;(add-hook 'rust-mode-hook #'racer-mode)
;;;(add-hook 'racer-mode-hook #'eldoc-mode)
;;;(add-hook 'racer-mode-hook #'company-mode)
;;;(setq company-tooltip-align-annotations t)
;;;(add-hook 'racer-mode-hook
;;;          '(lambda ()
;;;             (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
;;;             (set (make-local-variable 'company-backends) '(company-racer))
;;;             (local-set-key (kbd "TAB") #'racer-complete-or-indent)))

(provide 'init-local)
