(when *is-a-windows*
  ;; Chinese coding setting in Windons (copy and modify from init-locales.el)
  ;(set-language-environment 'chinese-GBK)
  ;(setq locale-coding-system 'gbk)
  ;(set-default-coding-systems 'gbk)
  ;(set-terminal-coding-system 'gbk)
  ;(set-selection-coding-system 'gbk)
  ;; Japanese coding setting in Windons (copy and modify from init-locales.el)
  ;(set-language-environment 'Japanese)
  ;(setq locale-coding-system 'japanese-shift-jis)
  ;(set-default-coding-systems 'japanese-shift-jis)
  ;(set-terminal-coding-system 'japanese-shift-jis)
  ;(set-selection-coding-system 'japanese-shift-jis)
  )
(setq session-save-file-coding-system 'utf-8)
;; auto detect file's encoding
(require 'unicad)

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

;;设置输入法列表
(setq my-input-methods
      '("japanese"
        "chinese-wbim"))
(defun my-cycle-input-method ()
  "Cycle `my-input-method-alist'."
  (interactive)
  (let ((cur-input-method (car my-input-methods)))
    (setq my-input-methods (append (cdr my-input-methods) (list cur-input-method)))
    (set-input-method cur-input-method)))
(global-set-key (kbd "M-C-\\") 'my-cycle-input-method)

(unless *source-view-only*
  (require 'calendar-fate)
  (calendar-fate-chinese-character)
  (setq holiday-other-holidays '((holiday-chinese-terms))))

;; ;(setq org-time-stamp-custom-formats '("<%Y/%m/%d>" . "<%Y/%m/%d %H:%M>"))

(defun thuleqaid/indent-tabs-mode-setup ()
  (setq indent-tabs-mode t))
(add-hook 'c-mode-hook 'thuleqaid/indent-tabs-mode-setup)
(add-hook 'text-mode-hook 'thuleqaid/indent-tabs-mode-setup)

(ac-config-default)
(add-hook 'python-mode-hook 'ac-cc-mode-setup)

(global-set-key (kbd "<f12>") 'highlight-symbol)
(global-set-key (kbd "M-<f12>") 'highlight-symbol-remove-all)

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
(global-unset-key (kbd "M-t"))
(global-set-key (kbd "M-c c") 'capitalize-word)
(global-set-key (kbd "M-c C") 'capitalize-region)
(global-set-key (kbd "M-c u") 'upcase-word)
(global-set-key (kbd "M-c U") 'upcase-region)
(global-set-key (kbd "M-c l") 'downcase-word)
(global-set-key (kbd "M-c L") 'downcase-region)
(global-unset-key (kbd "C-t"))
(global-set-key (kbd "M-c t") 'transpose-chars)
(global-set-key (kbd "M-c T") 'transpose-words)
(setq avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s))
(global-set-key (kbd "M-c F") 'avy-goto-char)
(global-set-key (kbd "M-c f") 'avy-goto-char-timer)
(global-unset-key (kbd "M-z"))
(global-unset-key (kbd "M-Z"))
(global-set-key (kbd "M-c Z") 'zap-to-char)
(global-set-key (kbd "M-c z") 'zap-up-to-char)
(global-set-key (kbd "M-c *") 'isearch-forward-symbol-at-point)
(global-set-key (kbd "M-c /") 'isearch-forward-regexp)
(defun thuleqaid/gotofile ()
  (interactive)
  (let* (
         (filename (ffap-file-exists-string (ffap-string-at-point)))
         (filenamelen (length filename))
         (jumpline 1))
    (if filename
        (progn
         (save-excursion
           (if (< (point) filenamelen)
               (goto-char (point-min))
             (backward-char filenamelen))
           (nonincremental-re-search-forward (concat (regexp-quote filename) "\\s-+"))
           (setq jumpline (thing-at-point 'number)))
         (ffap filename)
         (if jumpline (goto-line jumpline)))
    )))
(global-set-key (kbd "M-c g f") 'ffap)
(global-set-key (kbd "M-c g F") 'thuleqaid/gotofile)
(global-unset-key (kbd "C-M-="))
(global-unset-key (kbd "C-M--"))
(global-set-key (kbd "M-c +") 'default-text-scale-increase)
(global-set-key (kbd "M-c -") 'default-text-scale-decrease)
(global-set-key (kbd "M-c .") 'repeat)

(when (version< emacs-version "25")
  (setq tramp-mode nil)
  (setq tramp-ssh-controlmaster-options "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no"))

(require-package 'helm)
(require 'helm-config)
;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(unless *is-a-windows*
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  )
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
      )
(if *is-a-windows* (setq helm-gtags-path-style 'absolute))

;; Enable helm-gtags-mode in Dired so you can jump to any tag
;; when navigate project tree with Dired
(add-hook 'dired-mode-hook 'helm-gtags-mode)
;; Enable helm-gtags-mode in Eshell for the same reason as above
(add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
;; Set key bindings
(eval-after-load "helm-gtags"
  '(progn
     (define-key helm-gtags-mode-map (kbd "M-t b") 'helm-gtags-select)
     (define-key helm-gtags-mode-map (kbd "M-t t") 'helm-gtags-dwim)
     (define-key helm-gtags-mode-map (kbd "M-t s") 'helm-gtags-find-symbol)
     (define-key helm-gtags-mode-map (kbd "M-t f") 'helm-gtags-find-files)
     (define-key helm-gtags-mode-map (kbd "M-t <") 'helm-gtags-previous-history)
     (define-key helm-gtags-mode-map (kbd "M-t >") 'helm-gtags-next-history)
     (define-key helm-gtags-mode-map (kbd "M-t r") 'helm-gtags-resume)
     (define-key helm-gtags-mode-map (kbd "M-t p") 'helm-gtags-parse-file)
     (define-key helm-gtags-mode-map (kbd "M-t ,") 'helm-gtags-pop-stack)))
(helm-mode 1)

(provide 'init-local)
