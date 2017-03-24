(dolist (mode '(c-mode cc-mode c++-mode))
  (add-to-list 'ac-modes mode))
(add-hook 'c-mode-common-hook 'ac-cc-mode-setup)

(require-package 'ac-etags)
(require-package 'ctags-update)
(custom-set-variables
  '(ac-etags-requires 1))
(eval-after-load "etags"
  '(progn
      (ac-etags-setup)))
(add-hook 'c-mode-common-hook 'ac-etags-ac-setup)

(setq ctags-update-command (executable-find "ctags"))
(autoload 'turn-on-ctags-auto-update-mode "ctags-update" "turn on 'ctags-auto-update-mode'." t)
(add-hook 'c-mode-common-hook  'turn-on-ctags-auto-update-mode)
;; (autoload 'ctags-update "ctags-update" "update TAGS using ctags" t)

(defun thuleqaid/indent-tabs-mode-setup ()
  (setq indent-tabs-mode t)
  (c-set-style "stroustrup"))

(require-package 'helm-gtags)
;; Enable helm-gtags-mode
(setq helm-gtags-ignore-case t
      helm-gtags-auto-update t
      helm-gtags-use-input-at-cursor t
      helm-gtags-pulse-at-cursor t
      )
(if *is-a-windows* (setq helm-gtags-path-style 'absolute))

;; Set key bindings
(eval-after-load "helm-gtags"
  '(progn
     (evil-define-minor-mode-key 'normal 'helm-gtags-mode "\\gb" 'helm-gtags-select)
     (evil-define-minor-mode-key 'normal 'helm-gtags-mode "\\gt" 'helm-gtags-dwim)
     (evil-define-minor-mode-key 'normal 'helm-gtags-mode "\\gf" 'helm-gtags-find-files)
     (evil-define-minor-mode-key 'normal 'helm-gtags-mode "\\g<" 'helm-gtags-previous-history)
     (evil-define-minor-mode-key 'normal 'helm-gtags-mode "\\g>" 'helm-gtags-next-history)
     (evil-define-minor-mode-key 'normal 'helm-gtags-mode "\\gr" 'helm-gtags-resume)
     (evil-define-minor-mode-key 'normal 'helm-gtags-mode "\\gp" 'helm-gtags-parse-file)
     (evil-define-minor-mode-key 'normal 'helm-gtags-mode "\\g," 'helm-gtags-pop-stack)
     ))

;; get c include directories with "gcc -xc -E -v -"
;; get c++ include directories with "gcc -xc++ -E -v -"
;; use c++ include directories because c include directories is a subset of it
(require-package 'auto-complete-c-headers)
(defun thuleqaid/ac-c-header-init()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  (add-to-list 'achead:include-directories '"c:/PortableSoft/mingw32/bin/../lib/gcc/i686-w64-mingw32/6.2.0/include/c++")
  (add-to-list 'achead:include-directories '"c:/PortableSoft/mingw32/bin/../lib/gcc/i686-w64-mingw32/6.2.0/include/c++/i686-w64-mingw32")
  (add-to-list 'achead:include-directories '"c:/PortableSoft/mingw32/bin/../lib/gcc/i686-w64-mingw32/6.2.0/include/c++/backward")
  (add-to-list 'achead:include-directories '"c:/PortableSoft/mingw32/bin/../lib/gcc/i686-w64-mingw32/6.2.0/include")
  (add-to-list 'achead:include-directories '"c:/PortableSoft/mingw32/bin/../lib/gcc/i686-w64-mingw32/6.2.0/include-fixed")
  (add-to-list 'achead:include-directories '"c:/PortableSoft/mingw32/bin/../lib/gcc/i686-w64-mingw32/6.2.0/../../../../i686-w64-mingw32/include")
  )

(dolist (hook '(hide-ifdef-mode hs-minor-mode helm-gtags-mode
                thuleqaid/indent-tabs-mode-setup thuleqaid/ac-c-header-init))
  (add-hook 'c-mode-hook hook)
  (add-hook 'c++-mode-hook hook))

(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t
 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )

(provide 'init-c)
