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
	 ("C-c r" . counsel-recentf)
	 ("C-c g" . counsel-git)))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
	 ("C-r" . swiper-isearch-forward))
  :config (setq swiper-action-recenter t
		swiper-include-line-number-in-search t))

;; ivy-posframe is a ivy extension, which let ivy use posframe to show its candidate menu
(use-package ivy-posframe
  :init
  (setq ivy-posframe-display-functions-alist
	'((swiper . ivy-posframe-display-at-frame-center)
	  (complete-symbol . ivy-posframe-display-at-point)
	  (counse-M-x . ivy-posframe-display-at-frame-center)
	  (counsel-find-file . ivy-posframe-display-at-frame-center)
	  (ivy-switch-buffer . ivy-posframe-display-at-frame-center)
	  (t . ivy-posframe-display-at-frame-center)))
  (ivy-posframe-mode 1))

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
  :bind (("C-a" . 'mwim-beginning)
	 ("C-e" . 'mwim-end)))

;; Auto update packages
;; this maybe useful, if you want to update all the packages with command, just like me
(use-package auto-package-update
  :init (setq auto-package-update-delete-old-versions t
	      auto-package-update-hide-results t))

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
