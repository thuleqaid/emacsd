(require-package 'sr-speedbar)
(setq speedbar-show-unknown-files t)
(global-set-key (kbd "<f9>") (lambda()
                               (interactive)
                               (sr-speedbar-toggle)
                               (sr-speedbar-refresh)
                               (if (sr-speedbar-exist-p)
                                   (sr-speedbar-select-window)
                                 (kill-buffer "*SPEEDBAR*"))))

(defun thuleqaid/dired-open-in-external-app ()
  "Open the current file or dired marked files in external app.
The app is chosen from your OS's preference."
  (interactive)
  (let* ((file-list (dired-get-marked-files))
         (file-count (length file-list))
         (current-file (dired-get-filename))
         (open-list (if (or (<= file-count 0) (> file-count 5))
                        (list current-file)
                      file-list)))
    (cond
     ((string-equal system-type "windows-nt")
      (mapc
       (lambda (fPath)
         (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" fPath t t))) file-list))
     ((string-equal system-type "darwin")
      (mapc
       (lambda (fPath) (shell-command (format "open \"%s\"" fPath)))  file-list))
     ((string-equal system-type "gnu/linux")
      (mapc
       (lambda (fPath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" fPath))) file-list)))
    ))

(require-package 'diff-hl)

(after-load 'dired
  (define-key dired-mode-map "X"    'thuleqaid/dired-open-in-external-app)
  (when (fboundp 'global-dired-hide-details-mode)
    (global-dired-hide-details-mode -1))
  (setq dired-recursive-deletes 'top)
  (define-key dired-mode-map [mouse-2] 'dired-find-file)
  (add-hook 'dired-mode-hook
            (lambda () (guide-key/add-local-guide-key-sequence "%")))
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode))

(provide 'init-dired)
