;;; langs.el --- Language tooling: LSP/diagnostics/treesit -*- lexical-binding: t; -*-

;;; Commentary:
;; Language support: LSP server management, diagnostics, and treesit policy.

;;; Code:

(declare-function flymake-show-buffer-diagnostics "flymake")
(declare-function consult-flymake "consult")

(defvar my/eglot-auto-install-servers t
  "When non-nil, try to auto-install missing LSP servers for Eglot.")

(defvar my/eglot-mode-server-map
  '((python-mode . "ty")
    (python-ts-mode . "ty")
    (go-mode . "gopls")
    (go-ts-mode . "gopls")
    (rust-mode . "rust-analyzer")
    (rust-ts-mode . "rust-analyzer")
    (zig-mode . "zls")
    (zig-ts-mode . "zls")
    (c-mode . "clangd")
    (c-ts-mode . "clangd")
    (c++-mode . "clangd")
    (c++-ts-mode . "clangd"))
  "Major-mode to LSP server binary mapping.")

(defvar my/eglot-server-install-commands
  '(("ty" . "uv tool install --upgrade ty")
    ("gopls" . "go install golang.org/x/tools/gopls@latest")
    ("rust-analyzer" . "rustup component add rust-analyzer"))
  "Install command per LSP server binary.
Servers not listed here (e.g. clangd/zls) are expected to be installed manually.")

(defvar my/eglot-install-attempted (make-hash-table :test 'equal)
  "Track attempted server installs to avoid repeated installer runs.")

(defun my/eglot-current-server-binary ()
  "Return LSP server binary for current `major-mode', or nil."
  (alist-get major-mode my/eglot-mode-server-map))

(defun my/eglot-ensure-buffers-for-server (server)
  "Run `eglot-ensure' for open buffers that map to SERVER."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (derived-mode-p 'prog-mode)
                 (string= (or (my/eglot-current-server-binary) "") server)
                 (fboundp 'eglot-managed-p)
                 (not (eglot-managed-p))
                 (fboundp 'eglot-ensure))
        (ignore-errors (eglot-ensure))))))

(defun my/eglot-handle-install-exit (server process)
  "Handle PROCESS exit for SERVER auto-install.
Keep success markers, but clear failure markers to allow retries."
  (when (memq (process-status process) '(exit signal))
    (let ((code (process-exit-status process)))
      (if (zerop code)
          (progn
            (message "LSP server install finished: %s" server)
            ;; Retry connection for already-open buffers that previously failed.
            (my/eglot-ensure-buffers-for-server server))
        (remhash server my/eglot-install-attempted)
        (display-warning
         'langs
         (format "Auto-install failed for `%s` (exit=%s); will retry later" server code)
         :warning)))))

(defun my/eglot-maybe-install-server ()
  "Auto-install missing LSP server for current buffer when configured."
  (when my/eglot-auto-install-servers
    (when-let ((server (my/eglot-current-server-binary)))
      (unless (or (executable-find server) (gethash server my/eglot-install-attempted))
        (if-let ((cmd (alist-get server my/eglot-server-install-commands nil nil #'string=)))
            (condition-case err
                (let ((proc (start-process-shell-command
                             (format "eglot-install-%s" server)
                             "*eglot-server-install*"
                             cmd)))
                  (puthash server t my/eglot-install-attempted)
                  (set-process-sentinel
                   proc
                   (lambda (process _event)
                     (my/eglot-handle-install-exit server process)))
                  (message "Installing missing LSP server: %s" server))
              (error
               (remhash server my/eglot-install-attempted)
               (display-warning
                'langs
                (format "Failed to start auto-install for `%s`: %s"
                        server (error-message-string err))
                :warning)))
          (display-warning
           'langs
           (format "LSP server `%s' is missing; install it manually" server)
           :warning))))))

(defun my/eglot-auto-ensure ()
  "Install missing server if possible, then ensure Eglot is started."
  (my/eglot-maybe-install-server)
  (eglot-ensure))

;; Core LSP client (built-in) with a focused server mapping per language.
(use-package eglot
  :straight nil
  :commands (eglot eglot-ensure)
  :hook
  ((python-mode . my/eglot-auto-ensure)
   (python-ts-mode . my/eglot-auto-ensure)
   (go-mode . my/eglot-auto-ensure)
   (go-ts-mode . my/eglot-auto-ensure)
   (rust-mode . my/eglot-auto-ensure)
   (rust-ts-mode . my/eglot-auto-ensure)
   (zig-mode . my/eglot-auto-ensure)
   (zig-ts-mode . my/eglot-auto-ensure)
   (c-mode . my/eglot-auto-ensure)
   (c-ts-mode . my/eglot-auto-ensure)
   (c++-mode . my/eglot-auto-ensure)
   (c++-ts-mode . my/eglot-auto-ensure))
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

(defun my/flymake-diagnostics-panel-toggle ()
  "Toggle the diagnostics buffer for current source buffer in a side window."
  (interactive)
  (let ((buf-name (format "*Flymake diagnostics for `%s'*" (buffer-name)))
        (src-buf (current-buffer)))
    (if-let ((win (get-buffer-window buf-name)))
        (delete-window win)
      (flymake-show-buffer-diagnostics)
      (when-let ((diag-win (get-buffer-window buf-name)))
        (set-window-dedicated-p diag-win t)
        (set-window-buffer
         (display-buffer-in-side-window
          (window-buffer diag-win)
          '((side . right) (slot . 1) (window-width . 0.33)))
         (window-buffer diag-win))
        (select-window (get-buffer-window src-buf))))))

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
