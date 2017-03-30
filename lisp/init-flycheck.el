(when (maybe-require-package 'flycheck)
  (add-hook 'after-init-hook 'global-flycheck-mode)

  ;; Override default flycheck triggers
  (setq flycheck-check-syntax-automatically '(save mode-enabled))

  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))


(provide 'init-flycheck)
