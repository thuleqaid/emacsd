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


(setq vipath "C:/Program Files (x86)/Vim/vim80/gvim.exe")
(defun run-vi ()
  (interactive)
  (let ((exepath (or vipath
                    (executable-find "gvim")
                    (executable-find "vim")
                    (executable-find "vi")
                    ))
        )
    (when (file-executable-p exepath)
      ;; kill buffer when vim exists
      (async-start-process "Vim" exepath
                           (lambda (proc)
                             (kill-buffer (process-buffer proc))
                             ))
      )
    )
  )
(defun run-vi-readonly ()
  (interactive)
  (let ((exepath (or vipath
                    (executable-find "gvim")
                    (executable-find "vim")
                    (executable-find "vi")
                    ))
        )
    (when (file-executable-p exepath)
      ;; kill buffer when vim exists
      (async-start-process "Vim" exepath
                           (lambda (proc)
                             (kill-buffer (process-buffer proc))
                             )
                           "-R")
      )
    )
  )

(provide 'init-local)
