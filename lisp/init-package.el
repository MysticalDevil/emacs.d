;;; init-package.el --- initialize the plugins -*- lexical-binding: t -*-

;;; Commentary:
;;; Code:

(require 'evil)

;; A utility package to collect various Icon Fonts and propertize them within Emacs.
;; please install the non-free font Symbola. This issue usually occurs on Windows.
;; [Refs] https://github.com/seagle0128/doom-modeline
(use-package all-the-icons
  :when (display-graphic-p))

;; Nerd-icons.el is a library for easily using Nerd Font icons inside Emacs, an alternative to all-the-icons.
(use-package nerd-icons
  :when (display-graphic-p))

;; Auto update packages
;; this maybe useful, if you want to update all the packages with command, just like me
(use-package auto-package-update
  :init (setq auto-package-update-delete-old-versions t
              auto-package-update-hide-results t))

;; Make Emacs use the $PATH set up by the user's shell
(use-package exec-path-from-shell
  :defer nil
  :if (memq window-system '(mac ns x))
  :init (exec-path-from-shell-initialize))

;; Restart emacs from within emacs
(use-package restart-emacs)

;; Benchmarks for require and load calls
(use-package benchmark-init
  :init (benchmark-init/activate)
  :hook (after-init . benchmark-init/deactivate))

;; Tree sitter
(when (not *ts-avaiable*)
  (use-package tree-sitter-langs)
  (use-package tree-sitter
    :after tree-sitter-langs
    :config
    (global-tree-sitter-mode)
    (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)))

(when *ts-avaiable*
  (use-package treesit-auto
    :demand t
    :config
    (setq treesit-auto-install 'prompt)
    (global-treesit-auto-mode)))

;; A Collection of Ridiculously Useful eXtensions for Emacs
(use-package crux
  :bind (("C-c k" . crux-smart-kill-line))
  ("C-a" . crux-move-beginning-of-line)
  ("C-c ^" . crux-top-join-line)
  ("C-x ," . crux-find-user-init-file))

;; Enables hungry deletion in all modes.
(use-package hungry-delete
  :bind (("C-c DEL" . hungry-delete-backward))
  ("C-c d" . hungry-delete-forward))

;; move current line or region up or down
(use-package move-text
  :hook (after-init . move-text-default-bindings))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;; marginalia.el - Marginalia in the minibuffer
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

;; Treemacs - a tree layout file explorer for Emacs
(use-package treemacs
  :defer t
  :commands (treemacs-follow-mode
             treemacs-git-mode
             treemacs-filewatch-mode)
  :config
  (treemacs-git-mode 'deferred)
  (treemacs-filewatch-mode)
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t b"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag))
  (:map treemacs-mode-map
        ("/" . treemacs-advanced-helpful-hydra)
        ([mouse-1] . treemacs-single-click-expand-action))
  (:map evil-normal-state-map
        ("<leader> o p" . treemacs))
  :config
  (setq treemacs-missing-project-action 'remove
        treemacs-follow-after-init nil
        treemacs-show-hidden-files t
        treemacs-expand-after-init t)
  (treemacs-project-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (when treemacs-python-executable
    (treemacs-git-commit-diff-mode t))

  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple))))

(use-package treemacs-nerd-icons
    :demand t
    :when (icons-displayable-p)
    :custom-face
    (treemacs-nerd-icons-root-face ((t
                                     (:inherit nerd-icons-green :height 1.3))))
    (treemacs-nerd-icons-file-face ((t (:inherit nerd-icons-dsilver))))
    :config (treemacs-load-theme "nerd-icons"))

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-tab-bar
  :after (treemacs)
  :demand t
  :config (treemacs-set-scope-type 'Tabs))

;; treemacs-perspective if you use perspective.el vs. persp-mode
(use-package treemacs-persp
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-projectile
  :after (treemacs projectile))

;; A Git Porcelain inside Emacs
(use-package magit)

;; Enhanced the undo operate
(use-package undo-tree
  :init (global-undo-tree-mode)
  :custom (undo-tree-auto-save-history nil))

;; Emacs package that displays available keybindings in popup
(use-package which-key
  :defer nil
  :config (which-key-mode)
  (setq which-key-allow-evil-operators t))

;; An alternative M-x interface for Emacs.
(use-package amx
  :init (amx-mode))

;; Quickly switch windows in Emacs
(use-package ace-window
  :bind ("M-o" . 'ace-window))

;; Move to the beginning/end of line, code or comment
(use-package mwim
  :bind (("C-a" . 'mwim-beginning))
  ("C-e" . 'mwim-end))

;; format all, formatter for almost languages
;; great for programmers
(use-package format-all
  :diminish
  :hook (prog-mode . format-all-ensure-formatter)
  :bind ("C-c f" . #'format-all-buffer))

;; Show the delimiters as rainbow color
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
(use-package highlight-parentheses
  :hook (prog-mode . highlight-parentheses-mode))

;; In the blink of an eye, the search is complete
(use-package blink-search)

;; Emacs minor mode that keeps your code always indented.
;; More reliable than electric-indent-mode.
(use-package aggressive-indent-mode
  :hook ((emacs-lisp-mode . aggressive-indent-mode)
         (css-mode . aggressive-indent-mode)))

(provide 'init-package)
;;; init-package.el ends here
