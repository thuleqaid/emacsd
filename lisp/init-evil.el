(require-package 'evil)

(require-package 'avy)
(setq avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s))

(after-load 'evil
  (require-package 'evil-anzu)

  (require-package 'evil-args)
  ;; bind evil-args text objects
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)
  ;; bind evil-forward/backward-args
  (define-key evil-normal-state-map "L" 'evil-forward-arg)
  (define-key evil-normal-state-map "H" 'evil-backward-arg)
  (define-key evil-motion-state-map "L" 'evil-forward-arg)
  (define-key evil-motion-state-map "H" 'evil-backward-arg)
  ;; bind evil-jump-out-args
  (define-key evil-normal-state-map "K" 'evil-jump-out-args)

  (require-package 'evil-easymotion)
  (evilem-default-keybindings "SPC")

  (require-package 'evil-leader)
  (global-evil-leader-mode)

  (require-package 'evil-mc)
  (global-evil-mc-mode 1)

  (require-package 'evil-nerd-commenter)

  (require-package 'evil-surround)
  (global-evil-surround-mode 1)

  (evil-leader/set-key
    "ci" 'evilnc-comment-or-uncomment-lines
    ;"cl" 'evilnc-quick-comment-or-uncomment-to-the-line
    ;"ll" 'evilnc-quick-comment-or-uncomment-to-the-line
    "cc" 'evilnc-copy-and-comment-lines
    ;"cp" 'evilnc-comment-or-uncomment-paragraphs
    ;"cr" 'comment-or-uncomment-region
    ;"cv" 'evilnc-toggle-invert-comment-line-by-line
    ;"."  'evilnc-copy-and-comment-operator
    ;"\\" 'evilnc-comment-operator ; if you prefer backslash key
  )

  (evil-set-initial-state 'grep-mode 'emacs)

)

(evil-mode 1)

(provide 'init-evil)
