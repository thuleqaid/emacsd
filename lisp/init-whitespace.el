(setq-default show-trailing-whitespace t)

(require-package 'whitespace-cleanup-mode)
(global-whitespace-cleanup-mode t)

(global-set-key [remap just-one-space] 'cycle-spacing)

;; tab/whitespace/line number
(global-linum-mode 1)
(setq default-tab-width 4)
(setq tab-width 4)
(setq tab-stop-list '(4  8  12 16 20 24 28 32 36 40
                      44 48 52 56 60 64 68 72 76 80
                      84 88 92 96))
(setq whitespace-style '(face tabs space-mark))
(global-whitespace-mode t)

(require 'hl-line)
(custom-set-faces '(hl-line ((t (:inverse-video t)))))
(global-hl-line-mode t)
;; (blink-cursor-mode -1)

(provide 'init-whitespace)
