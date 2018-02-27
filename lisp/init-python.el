(require-package 'elpy)

(defun thuleqaid/turn-on-python-mode ()
  ;; use elpy-config to check required python packages
  (elpy-enable)
                                        ;(elpy-use-ipython)
  (setq python-shell-interpreter "jupyter"
        python-shell-interpreter-args "console --simple-prompt")

  ;; use flycheck not flymake with elpy
  (when (maybe-require-package 'flycheck nil t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode))
  )

(add-hook 'python-mode-hook 'thuleqaid/turn-on-python-mode)

(provide 'init-python)
