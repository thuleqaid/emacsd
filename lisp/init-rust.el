(require-package 'racer)
(require-package 'rust-mode)
(require-package 'ac-racer)

(setq racer-rust-src-path "~/.cargo/rustc-1.11.0/src")
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)

;;(add-hook 'racer-mode-hook #'company-mode)
;;(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
;;(setq company-tooltip-align-annotations t)

(provide 'init-rust)
