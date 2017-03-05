(require-package 'helm)
(require 'helm-config)
;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "M-x") 'helm-M-x)
;;(global-set-key (kbd "C-x C-f") 'helm-find-files)
(unless *is-a-windows*
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  )
(global-set-key (kbd "C-x b") 'helm-mini)

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-M-x-fuzzy-match                  t
      helm-buffers-fuzzy-match              t
      helm-recentf-fuzzy-match              t
      helm-semantic-fuzzy-match             t
      helm-imenu-fuzzy-match                t
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)
(require-package 'helm-gtags)
;; Enable helm-gtags-mode
(setq helm-gtags-ignore-case t
      helm-gtags-auto-update t
      helm-gtags-use-input-at-cursor t
      helm-gtags-pulse-at-cursor t
      )
(if *is-a-windows* (setq helm-gtags-path-style 'absolute))

;; Enable helm-gtags-mode in Dired so you can jump to any tag
;; when navigate project tree with Dired
(add-hook 'dired-mode-hook 'helm-gtags-mode)
;; Enable helm-gtags-mode in Eshell for the same reason as above
(add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
;; Set key bindings
(eval-after-load "helm-gtags"
  '(progn
     (define-key helm-gtags-mode-map (kbd "M-t b") 'helm-gtags-select)
     (define-key helm-gtags-mode-map (kbd "M-t t") 'helm-gtags-dwim)
     (define-key helm-gtags-mode-map (kbd "M-t s") 'helm-gtags-find-symbol)
     (define-key helm-gtags-mode-map (kbd "M-t f") 'helm-gtags-find-files)
     (define-key helm-gtags-mode-map (kbd "M-t <") 'helm-gtags-previous-history)
     (define-key helm-gtags-mode-map (kbd "M-t >") 'helm-gtags-next-history)
     (define-key helm-gtags-mode-map (kbd "M-t r") 'helm-gtags-resume)
     (define-key helm-gtags-mode-map (kbd "M-t p") 'helm-gtags-parse-file)
     (define-key helm-gtags-mode-map (kbd "M-t ,") 'helm-gtags-pop-stack)))
(helm-mode 1)

(provide 'init-helm)
