(require-package 'racer)
(require-package 'rust-mode)
(require-package 'ac-racer)
(require-package 'flycheck-rust)

(setq racer-rust-src-path (getenv "RUST_SRC_PATH"))
(defun my/racer-mode-hook ()
  (ac-racer-setup))
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook 'my/racer-mode-hook)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

(provide 'init-rust)
