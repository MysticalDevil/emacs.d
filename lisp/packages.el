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

;; Make Embark collections previewable via Consult.
(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Contextual actions for completion candidates.
(use-package embark
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

;; LSP symbol search via Consult (AST/semantic navigation experience).
(use-package consult-eglot
  :after (consult eglot)
  :commands (consult-eglot-symbols))

;; Discover key prefixes and transient keymaps.
(use-package which-key
  :init
  (setq which-key-idle-delay 0.5
        which-key-idle-secondary-delay 0.05
        which-key-sort-order #'which-key-key-order-alpha
        which-key-max-description-length 36)
  :config
  (which-key-mode 1))

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

;; Visual undo history tree.
(use-package undo-tree
  :init
  (setq undo-tree-auto-save-history nil
        undo-tree-enable-undo-in-region t
        undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t)
  :config
  (global-undo-tree-mode 1))

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

;; Use Nerd Font icons in Treemacs for clearer file-type visualization.
(condition-case err
    (use-package treemacs-nerd-icons
      :straight (treemacs-nerd-icons :type git :host github :repo "rainstormstudio/treemacs-nerd-icons")
      :after (treemacs nerd-icons)
      :config
      (treemacs-load-theme "nerd-icons"))
  (error
   (display-warning
    'packages
    (format "treemacs-nerd-icons unavailable: %s" (error-message-string err))
    :warning)))

;; Make Treemacs and Evil keybindings work together.
(use-package treemacs-evil
  :after (treemacs evil))

;; Dedicated symbols side panel.
(use-package imenu-list
  :commands (imenu-list-smart-toggle)
  :init
  (setq imenu-list-focus-after-activation t
        imenu-list-size 0.28
        imenu-list-position 'right))

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

(defun my/go-major-mode-auto ()
  "Use `go-ts-mode' when available and ready, otherwise fallback to `go-mode'."
  (interactive)
  (cond
   ((and (fboundp 'go-ts-mode)
         (fboundp 'treesit-available-p)
         (fboundp 'treesit-language-available-p)
         (treesit-available-p)
         (treesit-language-available-p 'go))
    (go-ts-mode))
   ((fboundp 'go-mode)
    (go-mode))
   ((fboundp 'go-ts-mode)
    (go-ts-mode))
   (t
    (fundamental-mode))))

(add-to-list 'auto-mode-alist '("\\.go\\'" . my/go-major-mode-auto))

;; Rust language support with tree-sitter first, classic mode fallback.
(defun my/rust-major-mode-auto ()
  "Use `rust-ts-mode' when available and ready, otherwise fallback to `rust-mode'."
  (interactive)
  (cond
   ((and (fboundp 'rust-ts-mode)
         (fboundp 'treesit-available-p)
         (fboundp 'treesit-language-available-p)
         (treesit-available-p)
         (treesit-language-available-p 'rust))
    (rust-ts-mode))
   ((fboundp 'rust-mode)
    (rust-mode))
   ((fboundp 'rust-ts-mode)
    (rust-ts-mode))
   (t
    (fundamental-mode))))

(add-to-list 'auto-mode-alist '("\\.rs\\'" . my/rust-major-mode-auto))

;; Zig language support with tree-sitter first, classic mode fallback.
(use-package zig-mode
  :straight (zig-mode :type git :host github :repo "ziglang/zig-mode")
  :defer t)

(use-package zig-ts-mode
  :defer t)

(defvar my/zig-enable-treesit t
  "When non-nil, prefer `zig-ts-mode' for Zig buffers.")

(defun my/zig-ts-workaround-grammar-mismatch ()
  "Avoid known zig tree-sitter comment-query incompatibilities.
Some zig grammar versions don't expose the `comment' node expected by
`zig-ts-mode', which can trigger `treesit-query-error' during redisplay.
Keep tree-sitter mode enabled, but drop the broken comment feature."
  (when (derived-mode-p 'zig-ts-mode)
    (setq-local treesit-font-lock-feature-list
                (mapcar (lambda (level) (remq 'comment level))
                        treesit-font-lock-feature-list))
    (when (fboundp 'treesit-font-lock-recompute-features)
      (treesit-font-lock-recompute-features))))

(add-hook 'zig-ts-mode-hook #'my/zig-ts-workaround-grammar-mismatch)

(defun my/zig-major-mode-auto ()
  "Use `zig-ts-mode' when available and ready, otherwise fallback to `zig-mode'."
  (interactive)
  (if (and my/zig-enable-treesit
           (fboundp 'zig-ts-mode)
           (fboundp 'treesit-available-p)
           (fboundp 'treesit-language-available-p)
           (treesit-available-p)
           (treesit-language-available-p 'zig))
      (condition-case err
          (zig-ts-mode)
        (error
         (message "zig-ts-mode failed (%s), fallback to zig-mode"
                  (error-message-string err))
         (zig-mode)))
    (zig-mode)))

(add-to-list 'auto-mode-alist '("\\.zig\\(?:\\.zon\\)?\\'" . my/zig-major-mode-auto))

(provide 'packages)
;;; packages.el ends here
