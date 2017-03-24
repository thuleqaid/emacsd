(setq additional-path (getenv "EMACS_ADD_PATH"))
(when additional-path
  (setenv "PATH" (concat (getenv "PATH") ";" additional-path))
  (setq exec-path (append exec-path (split-string additional-path ";"))))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-benchmarking) ;; Measure startup time

;;(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-windows* (eq system-type 'windows-nt))
(defconst *source-view-only* nil)

;;----------------------------------------------------------------------------
;; Temporarily reduce garbage collection during startup
;;----------------------------------------------------------------------------
(defconst sanityinc/initial-gc-cons-threshold gc-cons-threshold
  "Initial value of `gc-cons-threshold' at start-up time.")
(setq gc-cons-threshold (* 128 1024 1024))
(add-hook 'after-init-hook
          (lambda () (setq gc-cons-threshold sanityinc/initial-gc-cons-threshold)))

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(require 'init-utils)

(require 'init-elpa)      ;; Machinery for installing required packages

;;----------------------------------------------------------------------------
;; Allow users to provide an optional "init-preload-local.el"
;;----------------------------------------------------------------------------
(require 'init-preload-local nil t)

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------

(require-package 'wgrep)
(require-package 'project-local-variables)
(require-package 'diminish)
(require-package 'mwe-log-commands)

(require 'init-themes)
(require 'init-dired)
(require 'init-isearch)
(require 'init-grep)
(require 'init-ibuffer)
(unless *source-view-only*
  (require 'init-flycheck))

(require 'init-auto-complete)
(require 'init-sessions)
(require 'init-mmm)
(require 'init-helm)
(require 'init-evil)

(require 'init-editing-utils)
(require 'init-whitespace)
(require 'init-fci)

;; agenda path
(setq org-agenda-path (expand-file-name "agenda/" user-emacs-directory))
;; dairy path
(setq org-diary-path (expand-file-name user-emacs-directory))
;; system encoding
(setq ffmpeg-cmd-encoding 'japanese-shift-jis)
;; ffmpeg -list_devices true -f dshow -i dummy 2>devices.txt
(setq ffmpeg-audio-device "マイク配列 (Realtek High Definition ")

(unless *source-view-only*
  (require 'init-vc)
  (require 'init-compile)
  (require 'init-markdown)
  (require 'init-csv)
  (require 'init-javascript)
  (require 'init-html)
  (require 'init-css)
  (require 'init-lisp)
  (require 'init-python)
  (require 'init-c)
  ;; Require for init-rust: 1. `cargo install racer` and make sure racer in environment variable "PATH" 2. set environment variable "RUST_SRC_PATH" to rust source path, e.g. "c:\PortableSoft\home\rustc-1.13.0\src"
  (require 'init-rust)
  ;; Require for init-org: 1. set org-agenda-path and org-diary-path 2. set ffmpeg-cmd-encoding and ffmpeg-audio-device
  (require 'init-org))

(require 'init-misc)

;; Extra packages which don't require any configuration

(require-package 'gnuplot)
(require-package 'htmlize)
(require-package 'regex-tool)

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(require 'server)
(unless (server-running-p)
  (server-start))


;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(when (file-exists-p custom-file)
  (load custom-file))


;;----------------------------------------------------------------------------
;; Locales (setting them earlier in this file doesn't work in X)
;;----------------------------------------------------------------------------
(unless *is-a-windows*
  (require 'init-locales))

(add-hook 'after-init-hook
          (lambda ()
            (message "init completed in %.2fms"
                     (sanityinc/time-subtract-millis after-init-time before-init-time))))


;;----------------------------------------------------------------------------
;; Allow users to provide an optional "init-local" containing personal settings
;;----------------------------------------------------------------------------
(when (file-exists-p (expand-file-name "init-local.el" user-emacs-directory))
  (error "Please move init-local.el to ~/.emacs.d/lisp"))
(require 'init-local nil t)

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
