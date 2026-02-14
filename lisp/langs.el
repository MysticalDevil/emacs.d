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

;; Map major modes to tree-sitter language symbols.
(defvar my/treesit-mode-language-alist
  '((python-mode . python)
    (python-ts-mode . python)
    (go-mode . go)
    (go-ts-mode . go)
    (rust-mode . rust)
    (rust-ts-mode . rust)
    (zig-mode . zig)
    (zig-ts-mode . zig)
    (c-mode . c)
    (c-ts-mode . c)
    (c++-mode . cpp)
    (c++-ts-mode . cpp)
    (typescript-mode . typescript)
    (typescript-ts-mode . typescript))
  "Major-mode to tree-sitter language mapping.")

(defun my/treesit-language-for-current-buffer ()
  "Return the tree-sitter language symbol for current buffer, or nil."
  (alist-get major-mode my/treesit-mode-language-alist))

(defun my/treesit-install-current-language ()
  "Install tree-sitter grammar for the current buffer language."
  (interactive)
  (if (not (fboundp 'treesit-install-language-grammar))
      (message "Current Emacs build does not support tree-sitter")
    (let ((lang (my/treesit-language-for-current-buffer)))
      (cond
       ((null lang)
        (message "No tree-sitter language mapping for %s" major-mode))
       ((and (fboundp 'treesit-language-available-p)
             (treesit-language-available-p lang))
        (message "tree-sitter grammar for %s is already installed" lang))
       (t
        (treesit-install-language-grammar lang)
        (message "Installed tree-sitter grammar for %s" lang))))))

;; Install tree-sitter grammar for the language of the current file.
(global-set-key (kbd "C-c t i") #'my/treesit-install-current-language)

;; Prefer tree-sitter based major modes when grammar support exists.
(use-package treesit-auto
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode 1))

(provide 'langs)
;;; langs.el ends here
