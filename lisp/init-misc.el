;;----------------------------------------------------------------------------
;; Misc config - yet to be placed in separate files
;;----------------------------------------------------------------------------
(fset 'yes-or-no-p 'y-or-n-p)

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(setq-default regex-tool-backend 'perl)

;; backup setting
(let ((backup-path (expand-file-name "backup/" user-emacs-directory))
      )
  (if (not (file-accessible-directory-p backup-path))
      (make-directory backup-path)
    )
  (setq backup-directory-alist (list (cons "" backup-path)))
  )
(setq-default make-backup-file t)
(setq make-backup-file t)
(setq make-backup-files t)
(setq version-control t)
(setq kept-old-versions 2)
(setq kept-new-versions 10)
(setq delete-old-versions t)

(auto-image-file-mode t)
(iimage-mode t)

;; time display
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time)

(require-package 'yasnippet)
(yas-global-mode 1)

(provide 'init-misc)
