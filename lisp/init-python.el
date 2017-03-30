(require-package 'elpy)

;; use elpy-config to check required python packages
(elpy-enable)
(elpy-use-ipython)

;; use flycheck not flymake with elpy
(when (maybe-require-package 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; enable autopep8 formatting on save
(require-package 'py-autopep8)
;;; py-autopep8 default action: patch diffs between original file and temp file outputed by autopep8
;;;   default action will cause additional ^M when editing unix format file in windows
;; (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
(require 'py-autopep8)
(add-hook 'elpy-mode-hook (lambda ()
                            (add-hook 'before-save-hook
                                      (lambda ()
                                        (mark-whole-buffer)
                                        (py-autopep8-bf--apply-executable-to-buffer
                                         "autopep8"
                                         'py-autopep8--call-executable
                                         t
                                         "py"))
                                      nil t)))

(provide 'init-python)
