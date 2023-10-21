;;; init-package.el -- settings for packages

;;; Commentary:
;;; Code:

;; Restart emacs from within emacs
(use-package restart-emacs)

;; Future-proof your Emacs Lisp customizations!
(use-package el-patch)

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

;; Use y_n to replace yes_no
(use-package emacs
  :config (defalias 'yes-or-no-p 'y-or-n-p))

;; A Collection of Ridiculously Useful eXtensions for Emacs
(use-package crux
  :bind (("C-c k" . crux-smart-kill-line)
	 ("C-a" . crux-move-beginning-of-line)
	 ("C-c ^" . crux-top-join-line)
	 ("C-x ," . crux-find-user-init-file)))

;; Enables hungry deletion in all modes.
(use-package hungry-delete
  :bind (("C-c DEL" . hungry-delete-backward)
	 ("C-c d" . hungry-delete-forward)))

;; Drag stuff around in Emacs
(use-package drag-stuff
  :bind (("<M-up>" . drag-stuff-up)
	 ("<M-down>" . drag-stuff-down)))

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
  :bind (("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)
	 ("C-c f" . counsel-recentf)
	 ("C-c g" . counsel-git)))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
	 ("C-r" . swiper-isearch-forward))
  :config (setq swiper-action-recenter t
		swiper-include-line-number-in-search t))

;; Modular in-buffer completion framework for Emacs
(use-package company
  :config
  (setq company-dabbrev-code-everywhere t
	company-dabbrev-code-modes t
	company-dabbrev-code-other-buffers 'all
	company-dabbrev-downcase nil
	company-dabbrev-ignore-case t
	company-dabbrev-other-buffers 'all
	company-require-match nil
	company-minimum-prefix-length 2
	company-show-numbers t
	company-tooltip-limit 20
	company-idle-delay 0
	company-echo-dely 0
	company-tooltip-offset-display 'scrollbar
	company-begin-commands '(self-insert-command))
  (push '(company0semantic :with company-yasnippet) company-backends)
  :hook ((after-init . global-company-mode)))

;; On-the-fly syntax checking
(use-package flycheck
  :config (setq truncate-lines nil)
  :hook (prog-mode . flycheck-mode))

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

(provide 'init-package)
;;; init-package.el ends here
