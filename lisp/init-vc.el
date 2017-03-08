(require-package 'diff-hl)
(add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
(add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode)

(require-package 'mo-git-blame)
(require-package 'gitignore-mode)
(require-package 'gitconfig-mode)
(require-package 'git-messenger)
(require-package 'git-timemachine)

(autoload 'mo-git-blame-file "mo-git-blame" nil t)
(autoload 'mo-git-blame-current "mo-git-blame" nil t)
(eval-after-load 'mo-git-blame
  '(progn
     (evil-make-overriding-map mo-git-blame-mode-map 'normal)
     ;; force update evil keymaps after mo-git-blame-mode loaded
     (add-hook 'mo-git-blame-mode-hook #'evil-normalize-keymaps)))

(when (maybe-require-package 'magit)
  (setq-default
   magit-process-popup-time 10
   magit-diff-refine-hunk t
   magit-completing-read-function 'magit-ido-completing-read))

(after-load 'magit
  (add-hook 'magit-popup-mode-hook 'sanityinc/no-trailing-whitespace))

(require-package 'fullframe)
(after-load 'magit
  (fullframe magit-status magit-mode-quit-window))

(when (maybe-require-package 'git-commit)
  (add-hook 'git-commit-mode-hook 'goto-address-mode))

(eval-after-load 'git-timemachine
  '(progn
     (evil-make-overriding-map git-timemachine-mode-map 'normal)
     ;; force update evil keymaps after git-timemachine-mode loaded
     (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps)))

;;; git-svn support
;; (when (maybe-require-package 'magit-svn)
;;   (require-package 'magit-svn)
;;   (autoload 'magit-svn-enabled "magit-svn")
;;   (defun sanityinc/maybe-enable-magit-svn-mode ()
;;     (when (magit-svn-enabled)
;;       (magit-svn-mode)))
;;   (add-hook 'magit-status-mode-hook #'sanityinc/maybe-enable-magit-svn-mode))
;;(after-load 'compile
;;  (dolist (defn (list '(git-svn-updated "^\t[A-Z]\t\\(.*\\)$" 1 nil nil 0 1)
;;                      '(git-svn-needs-update "^\\(.*\\): needs update$" 1 nil nil 2 1)))
;;    (add-to-list 'compilation-error-regexp-alist-alist defn)
;;    (add-to-list 'compilation-error-regexp-alist (car defn))))
;;(defvar git-svn--available-commands nil "Cached list of git svn subcommands")
;;(defun git-svn--available-commands ()
;;  (or git-svn--available-commands
;;      (setq git-svn--available-commands
;;            (sanityinc/string-all-matches
;;             "^  \\([a-z\\-]+\\) +"
;;             (shell-command-to-string "git svn help") 1))))
;;(defun git-svn (dir command)
;;  "Run a git svn subcommand in DIR."
;;  (interactive (list (read-directory-name "Directory: ")
;;                     (completing-read "git-svn command: " (git-svn--available-commands) nil t nil nil (git-svn--available-commands))))
;;  (let* ((default-directory (vc-git-root dir))
;;         (compilation-buffer-name-function (lambda (major-mode-name) "*git-svn*")))
;;    (compile (concat "git svn " command))))

(provide 'init-vc)
