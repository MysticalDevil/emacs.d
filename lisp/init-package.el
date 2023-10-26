;;; init-package.el --- initialize the plugins -*- lexical-binding: t -*-

;;; Commentary:
;;; Code:

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

;; Ivy - a generic completion frontend for Emacs, Swiper - isearch with an overview, and more. Oh, man!
(use-package ivy
  :defer 1
  :demand
  :hook (after-init . ivy-mode)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-initial-inputs-alist nil
        ivy-count-format "[%d/%d]"
        enable-recursive-minibuffers t
        ivy-re-builders-alist '((t . ivy--regex-ignore-order))))

(use-package counsel
  :after (ivy)
  :bind (("M-x" . counsel-M-x))
  ("C-x C-f" . counsel-find-file)
  ("C-c r" . counsel-recentf)
  ("C-c g" . counsel-git))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper))
  ("C-r" . swiper-isearch)
  :config (setq swiper-action-recenter t
                swiper-include-line-number-in-search t))

(use-package ivy-rich
  :hook (ivy-mode . ivy-rich-mode)
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

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
  (treemacs-load-theme "all-the-icons")
  :bind
  (:map global-map
        ("C-c o p"   . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ;; ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag))
  (:map treemacs-mode-map
        ("/" . treemacs-advanced-helpful-hydra)
        ([mouse-1] . treemacs-single-click-expand-action))
  :config
  (setq treemacs-missing-project-action 'remove
        treemacs-follow-after-init t)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t))


(use-package treemacs-all-the-icons
  :after (treemacs)
  :config (treemacs-all-the-icons-mode))

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after (treemacs magit)
  :config (treemacs-magit-mode))

(use-package treemacs-tab-bar
  :demand t
  :config (treemacs-set-scope-type 'Tabs))

;; A Git Porcelain inside Emacs
(use-package magit)

;; Enhanced the undo operate
(use-package undo-tree
  :init (global-undo-tree-mode)
  :custom (undo-tree-auto-save-history nil))

;; Emacs package that displays available keybindings in popup
(use-package which-key
  :defer nil
  :config (which-key-mode))

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
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))
(use-package highlight-parentheses
  :init (add-hook 'prog-mode-hook 'highlight-parentheses-mode))

(provide 'init-package)
;;; init-package.el ends here
