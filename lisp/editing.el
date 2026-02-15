;;; editing.el --- Formatting and in-buffer completion -*- lexical-binding: t; -*-

(declare-function cape-file "cape")
(declare-function cape-dabbrev "cape")
(declare-function cape-keyword "cape")
(declare-function cape-yasnippet "cape")
(declare-function yasnippet-capf "yasnippet")
(declare-function parinfer-rust-mode "parinfer-rust-mode")

;; Keep indentation behavior predictable while still allowing CAPF completion.
(setq tab-always-indent 'complete)

;; Keep long code lines on one visual line (horizontal scroll instead of wrapping).
(defun my/prog-no-wrap ()
  "Disable soft wrapping in programming buffers."
  (setq-local truncate-lines t
              word-wrap nil))

(add-hook 'prog-mode-hook #'my/prog-no-wrap)

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
   ((and (fboundp 'apheleia-format-buffer)
         (fboundp 'apheleia--get-formatters))
    (if-let ((formatters (apheleia--get-formatters)))
        (apheleia-format-buffer formatters)
      (message "No Apheleia formatter configured for current buffer")))
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

(defun my/add-capf-if-available (fn depth)
  "Add completion FN to current buffer when it is available.
DEPTH controls hook ordering for `completion-at-point-functions'."
  (when (fboundp fn)
    (add-hook 'completion-at-point-functions fn depth t)))

(defun my/setup-cape-capf ()
  "Add useful Cape completion sources to current buffer."
  (cond
   ((fboundp 'cape-yasnippet)
    (my/add-capf-if-available #'cape-yasnippet -30))
   ((fboundp 'yasnippet-capf)
    (my/add-capf-if-available #'yasnippet-capf -30)))
  (my/add-capf-if-available #'cape-file -20)
  (my/add-capf-if-available #'cape-dabbrev -10)
  (my/add-capf-if-available #'cape-keyword 0))

;; Extra completion sources for CAPF.
(use-package cape
  :init
  (add-hook 'prog-mode-hook #'my/setup-cape-capf))

;; Snippet engine and community snippet collection.
(use-package yasnippet
  :init
  (setq yas-triggers-in-field t)
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)

;; Structured editing for brackets/quotes in code and minibuffer.
(use-package smartparens
  :hook
  ((prog-mode . smartparens-mode)
   (minibuffer-setup . smartparens-mode))
  :config
  (require 'smartparens-config))

(defun my/enable-parinfer-rust-mode ()
  "Enable Parinfer and disable Smartparens in current Lisp buffer."
  (setq-local indent-tabs-mode nil)
  (when (bound-and-true-p smartparens-mode)
    (smartparens-mode -1))
  (parinfer-rust-mode 1))

;; Parinfer for Lisp-style editing (best paired with modal editing muscle memory).
(use-package parinfer-rust-mode
  :hook
  ((emacs-lisp-mode . my/enable-parinfer-rust-mode)
   (lisp-mode . my/enable-parinfer-rust-mode)
   (lisp-interaction-mode . my/enable-parinfer-rust-mode)
   (lisp-data-mode . my/enable-parinfer-rust-mode)
   (clojure-mode . my/enable-parinfer-rust-mode)
   (scheme-mode . my/enable-parinfer-rust-mode))
  :init
  (setq parinfer-rust-auto-download t))

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
