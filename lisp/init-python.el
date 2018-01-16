(require-package 'elpy)

;; use elpy-config to check required python packages
(elpy-enable)
;(elpy-use-ipython)
(setq python-shell-interpreter "jupyter"
      python-shell-interpreter-args "console --simple-prompt")

;; use flycheck not flymake with elpy
(when (maybe-require-package 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))


(provide 'init-python)
