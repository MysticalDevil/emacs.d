;;; langs.el --- Language tooling: LSP/diagnostics/treesit -*- lexical-binding: t; -*-

;; Core LSP client (built-in) with a focused server mapping per language.
(use-package eglot
  :straight nil
  :commands (eglot eglot-ensure)
  :hook
  ((python-mode . eglot-ensure)
   (python-ts-mode . eglot-ensure)
   (go-mode . eglot-ensure)
   (go-ts-mode . eglot-ensure)
   (rust-mode . eglot-ensure)
   (rust-ts-mode . eglot-ensure)
   (zig-mode . eglot-ensure)
   (zig-ts-mode . eglot-ensure)
   (c-mode . eglot-ensure)
   (c-ts-mode . eglot-ensure)
   (c++-mode . eglot-ensure)
   (c++-ts-mode . eglot-ensure))
  :config
  ;; Keep server startup deterministic and explicit.
  (dolist (entry '(((python-mode python-ts-mode) . ("ty" "server"))
                   ((go-mode go-ts-mode) . ("gopls"))
                   ((rust-mode rust-ts-mode) . ("rust-analyzer"))
                   ((zig-mode zig-ts-mode) . ("zls"))
                   ((c-mode c-ts-mode c++-mode c++-ts-mode) . ("clangd"))))
    (add-to-list 'eglot-server-programs entry)))

;; Diagnostics backend (built-in); set a stable display policy.
(use-package flymake
  :straight nil
  :hook (prog-mode . flymake-mode)
  :bind
  (("C-c ! n" . flymake-goto-next-error)
   ("C-c ! p" . flymake-goto-prev-error))
  :init
  (setq flymake-fringe-indicator-position 'right-fringe
        flymake-suppress-zero-counters t
        flymake-no-changes-timeout 0.3)
  :config
  ;; End-of-line diagnostics are clearer for quick fixes when available.
  (when (fboundp 'flymake-show-diagnostics-at-end-of-line-mode)
    (add-hook 'flymake-mode-hook #'flymake-show-diagnostics-at-end-of-line-mode)))

;; Consult integration for buffer-wide diagnostics navigation.
(with-eval-after-load 'consult
  (global-set-key (kbd "C-c ! l") #'consult-flymake))

;; Prefer tree-sitter based major modes when grammar support exists.
(use-package treesit-auto
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode 1))

(provide 'langs)
;;; langs.el ends here
