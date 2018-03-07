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

;;添加模版
(auto-insert-mode)  ;;; Adds hook to find-files-hook
(setq auto-insert-directory "~/.emacs.d/templates/")
(setq auto-insert-query nil)
(setq auto-insert-alist
      (append '(
                (org-mode . "template.org")
                (python-mode . "template.py")
                (rst-mode . "template.rst")
                )
              auto-insert-alist))

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
