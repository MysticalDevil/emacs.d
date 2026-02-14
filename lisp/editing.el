;;; editing.el --- Formatting and in-buffer completion -*- lexical-binding: t; -*-

;; Keep indentation behavior predictable while still allowing CAPF completion.
(setq tab-always-indent 'complete)

;; Formatters fallback for languages without LSP formatting support.
(use-package apheleia
  :config
  ;; Enable in programming buffers as a formatter fallback.
  (add-hook 'prog-mode-hook #'apheleia-mode))

(defun my/eglot-format-available-p ()
  "Return non-nil when current buffer can be formatted by Eglot."
  (and (featurep 'eglot)
       (fboundp 'eglot-managed-p)
       (eglot-managed-p)
       (fboundp 'eglot-server-capable)
       (eglot-server-capable :documentFormattingProvider)))

(defun my/format-buffer-dwim ()
  "Format current buffer with LSP first, otherwise fallback to Apheleia."
  (interactive)
  (cond
   ;; Prefer LSP formatter when server advertises document formatting.
   ((and (my/eglot-format-available-p)
         (fboundp 'eglot-format-buffer))
    (eglot-format-buffer))
   ;; Fallback formatter for non-LSP or formatter-missing cases.
   ((fboundp 'apheleia-format-buffer)
    (apheleia-format-buffer))
   (t
    (message "No formatter available for current buffer"))))

(defun my/enable-format-on-save ()
  "Use `my/format-buffer-dwim' before saving in current buffer."
  (add-hook 'before-save-hook #'my/format-buffer-dwim nil t))

;; Auto-format source buffers on save.
(add-hook 'prog-mode-hook #'my/enable-format-on-save)

;; Completion popup UI for in-buffer completion-at-point.
(use-package corfu
  :init
  (setq corfu-auto t
        corfu-auto-delay 0.12
        corfu-auto-prefix 2
        corfu-cycle t
        corfu-preview-current nil)
  :config
  (global-corfu-mode 1))

;; Extra completion sources for CAPF.
(use-package cape
  :init
  (defun my/setup-cape-capf ()
    "Add useful Cape completion sources to current buffer."
    (add-hook 'completion-at-point-functions #'cape-file -20 t)
    (add-hook 'completion-at-point-functions #'cape-dabbrev -10 t)
    (add-hook 'completion-at-point-functions #'cape-keyword 0 t))
  (add-hook 'prog-mode-hook #'my/setup-cape-capf))

;; Structured editing for brackets/quotes in code and minibuffer.
(use-package smartparens
  :hook
  ((prog-mode . smartparens-mode)
   (minibuffer-setup . smartparens-mode))
  :config
  (require 'smartparens-config))

;; Vim-like surround motions/operators.
(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

;; Better comment toggling for line/region.
(use-package evil-nerd-commenter
  :commands (evilnc-comment-or-uncomment-lines)
  :bind
  (("M-/" . evilnc-comment-or-uncomment-lines)))

;; Highlight TODO/FIXME/NOTE style annotations.
(use-package hl-todo
  :hook
  ((prog-mode . hl-todo-mode)
   (text-mode . hl-todo-mode)))

;; Keep diffs clean by trimming trailing whitespace on save.
(use-package ws-butler
  :hook
  ((prog-mode . ws-butler-mode)
   (text-mode . ws-butler-mode)))

;; Incremental semantic region expansion (similar to visual selection growth).
(use-package expand-region
  :bind
  (("C-=" . er/expand-region)))

(provide 'editing)
;;; editing.el ends here
