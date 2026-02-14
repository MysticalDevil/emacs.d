;;; packages.el --- Package configuration (Vertico stack) -*- lexical-binding: t; -*-

;; Keep minibuffer history across sessions (built-in).
(use-package savehist
  :straight nil
  :init
  (setq history-length 200)
  (savehist-mode 1))

;; Provide minibuffer completion UI.
(use-package vertico
  :init
  (setq vertico-cycle t
        vertico-resize t)
  :config
  (vertico-mode 1))

;; Better directory navigation in Vertico minibuffer.
(use-package vertico-directory
  :straight nil
  :after vertico
  :bind (:map vertico-map
              ("M-DEL" . vertico-directory-delete-word)
              ("DEL"   . vertico-directory-delete-char)
              ("RET"   . vertico-directory-enter)))

;; Rich annotations in minibuffer completions.
(use-package marginalia
  :config
  (marginalia-mode 1))

;; Modern matching style for minibuffer completion.
(use-package orderless
  :init
  ;; Orderless works best when used as the primary completion style.
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        ;; Keep file completion sane (partial-completion keeps paths ergonomic).
        completion-category-overrides '((file (styles partial-completion)))))

;; High-value commands: buffer switching, search, imenu, ripgrep, etc.
(use-package consult
  :init
  ;; Reduce preview noise and keep it responsive.
  (setq consult-preview-key "M-."
        register-preview-delay 0.3
        register-preview-function #'consult-register-format)
  :config
  ;; Optional: automatically preview in *Completions* where relevant.
  (advice-add #'register-preview :override #'consult-register-window))

;; Contextual actions for completion candidates.
(use-package embark
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

;; Make Embark collections previewable via Consult.
(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Vim-style modal editing.
(use-package evil
  :init
  ;; Must be set before loading Evil.
  (setq evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-i-jump nil)
  :config
  (evil-mode 1))

;; Extend Evil bindings into common built-in/third-party modes.
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Built-in project management baseline.
(use-package project
  :straight nil
  :init
  (setq project-switch-commands
        '((project-find-file "Find file" ?f)
          (consult-ripgrep "Ripgrep" ?s)
          (consult-project-buffer "Project buffer" ?b)
          (project-dired "Dired" ?d))))

;; File tree for project navigation.
(use-package treemacs
  :defer t
  :init
  (setq treemacs-width 34
        treemacs-follow-after-init t
        treemacs-is-never-other-window t)
  :config
  (treemacs-filewatch-mode 1)
  (treemacs-follow-mode 1))

;; Make Treemacs and Evil keybindings work together.
(use-package treemacs-evil
  :after (treemacs evil))

;; Git porcelain and forge integrations.
(use-package magit
  :commands (magit-status magit-file-dispatch magit-blame-addition)
  :init
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; PR/MR and issue workflows from Magit.
(use-package forge
  :after magit)

;; VC diff markers in fringe and Magit refresh integration.
(use-package diff-hl
  :hook
  ((prog-mode . diff-hl-mode)
   (text-mode . diff-hl-mode)
   (dired-mode . diff-hl-dired-mode))
  :config
  (diff-hl-flydiff-mode 1)
  (with-eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

(provide 'packages)
;;; packages.el ends here
