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
  :bind ("C-c k" . crux-smart-kill-line))

;; Enables hungry deletion in all modes.
(use-package hungry-delete
  :bind ("C-c DEL" . hungry-delete-backward)
  :bind ("C-c d" . hungry-delete-forward))

(provide 'init-package)
