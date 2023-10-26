;;; init-ide.el --- Programming mode about settings -*- lexical-binding: t -*-

;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; settings for LSP MODE ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eglot
  :hook ((c-mode c++-mode go-mode js2-mode python-mode rust-mode web-mode) . eglot-ensure)
  :bind (("C-c e f" . #'eglot-format)
         ("C-c e i" . #'eglot-code-action-organize-imports)
         ("C-c e q" . #'eglot-code-action-qucik-fix))
  :config
  ;; (setq eglot-ignored-server-capabilities '(:documentHighlightProvider))
  (defun eglot-actions-before-save ()
    (add-hook 'before-save-hook (lambda ()
                                  (call-interactively #'eglot-format)
                                  (call-interactively #'eglot-code-action-organize-imports))))
  (add-to-list 'eglot-server-programs '(web-mode "vls"))
  (add-hook 'eglot--managed-mode-hook #'eglot-actions-before-save))

;; Modular in-buffer completion framework for Emacs
(use-package company
  :custom
  (company-dabbrev-code-everywhere t)
  (company-dabbrev-code-modes t)
  (company-dabbrev-code-other-buffers 'all)
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case t)
  (company-dabbrev-other-buffers 'all)
  (company-require-match nil)
  (company-minimum-prefix-length 2)
  (company-show-quick-access t)
  (company-tooltip-limit 20)
  (company-tooltip-align-annotations t)
  (company-idle-delay 0)
  (company-echo-dely 0)
  (company-tooltip-offset-display 'scrollbar)
  (company-begin-commands '(self-insert-command))
  (company-tempo-expand t)
  (company-backends '(company-capf company-files company-dabbrev))
  :config
  (push '(company-semantic :with company-yasnippet) company-backends)
  :hook (prog-mode . company-mode))

;; A company front-end with icons
(use-package orderless
  :init
  (setq completion-styles '(basic substring partial-completion flex)
        completion-category-defaults nil)
  :config
  (orderless-define-completion-style orderless+initialism
                                     (order-matching-styles '(orderless-initialism orderless-literal orderless-regexp)))
  (setq completion-category-overrides '((command (styles orderless+initialism))
                                        (symbol (styles orderless+initialism))
                                        (variable (styles orderless+initialism)))))

;; A template system for Emacs
(use-package yasnippet
  :after company
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all)
  ;; add company-yasnippet to company-backends
  (defun company-mode/backend-with-yas (backend)
    (if (and (listp backend) (member 'company-yasnippet backend)
             backend)
        (append (if (consp backend) backend (list backend))
                '(:with company-yasnippet))))
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
  ;; unbind <TAB> completion
  (define-key yas-minor-mode-map [(tab)]        nil)
  (define-key yas-minor-mode-map (kbd "TAB")    nil)
  (define-key yas-minor-mode-map (kbd "<tab>")  nil)
  :bind
  (:map yas-minor-mode-map ("S-<tab>" . yas-expand)))

(use-package yasnippet-snippets
  :after yasnippet)

;; Project Interaction Library for Emacs
(use-package projectile
  :bind (("C-c p" . projectile-command-map))
  :config
  (setq projectile-mode-line "Projectile")
  (setq projectile-track-known-projects-automatically nil))

(use-package counsel-projectile
  :after (projectile)
  :init (counsel-projectile-mode))

(use-package treemacs-projectile
  :after (treemacs projectile))

;; Flycheck
(use-package flycheck
  :hook (prog-mode . flycheck-mode))

(use-package flycheck-color-mode-line
  :after (flycheck))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; settings for Program Languages ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Lisp
;; Short and sweet LISP editing
(use-package lispy
  :init (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1))))
(use-package parinfer-rust-mode
  :hook emacs-lisp-mode
  :init (setq parinfer-rust--ask-to-download t))

;; Python
(use-package python
  :defer t
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python3" . python-mode)
  :config
  ;; for debug
  (require 'dap-python))

(use-package pyvenv
  :config
  (setq python-shell-interpreter "python3")
  (pyvenv-mode t))

(defmacro check-run-execute (exec-file &rest body)
  "Find the EXEC-FILE and run the BODY."

  `(if (not (executable-find ,exec-file))
       (message "[ERROR]: <%s> not found!" ,exec-file)
     ,@body))

;;;###autoload
(defun python-isort ()
  "Sort the imports with isort."
  (interactive)
  (check-run-execute "isort"
                     (shell-command-on-region
                      (point-min) (point-max)
                      "isort --atomic --profile=black -"
                      (current-buffer) t)))

;;;###autoload
(defun python-remove-all-unused-imports ()
  "Remove all the unused imports, do NOT use pyimport, as it has bugs.
eg.from datetime import datetime."
  (interactive)
  (check-run-execute "autoflake"
                     (shell-command
                      (format "autoflake -i --remove-all-unused-imports %s" (buffer-file-name)))
                     (revert-buffer t t t)))

(add-hook 'python-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'python-isort nil t)
            (define-key python-mode-map (kbd "C-c p s") 'python-isort)
            (define-key python-mode-map (kbd "C-c p r") 'python-remove-all-unused-imports)))

;; Go
(use-package go-mode)

;; Rust
(use-package rust-mode
  :functions dap-register-debug-template
  :bind
  ("C-c C-c" . rust-run)
  :hook
  (rust-mode . lsp-deferred)
  :config
  ;; debug
  (require 'dap-gdb-lldb)
  (dap-register-debug-template "Rust::LLDB Run Configuration"
                               (list :type "lldb"
                                     :request "launch"
                                     :name "rust-lldb::Run"
                                     :gdbpath "rust-lldb"
                                     :target nil
                                     :cwd nil)))

(use-package cargo
  :hook
  (rust-mode . cargo-minor-mode))

;; Web developmenter
(use-package web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
  :config (setq web-mode-enable-current-element-highlight t))

;; Use C-j to expand emmet
(use-package emmet-mode
  :hook ((web-mode . emmet-mode)
         (css-mode . emmet-mode)))

(use-package json-mode)
(use-package markdown-mode)
(use-package restclient
  :mode (("\\.http'" . restclient-mode)))
(use-package yaml-mode)

(use-package quickrun)

(provide 'init-prog)
;;; init-prog.el ends here
