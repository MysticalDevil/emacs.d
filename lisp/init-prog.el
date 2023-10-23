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
         ("C-c e q" . #'eglot-code-action-quick-fix))
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
  :config
  (setq company-dabbrev-code-everywhere t
	company-dabbrev-code-modes t
	company-dabbrev-code-other-buffers 'all
	company-dabbrev-downcase nil
	company-dabbrev-ignore-case t
	company-dabbrev-other-buffers 'all
	company-require-match nil
	company-minimum-prefix-length 2
	company-show-quick-access t
	company-tooltip-limit 20
	company-idle-delay 0
	company-echo-dely 0
	company-tooltip-offset-display 'scrollbar
	company-begin-commands '(self-insert-command))
  (push '(company-semantic :with company-yasnippet) company-backends)
  :hook ((after-init . global-company-mode)))

;; A template system for Emacs
(use-package yasnippet
  :after company
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all)
  ;; add company-yasnippet to company-backends
  (defun company-mode/backend-with-yas (backend)
    (if (and (listp backend) (member 'company-yasnippet backend))
	backend
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

;; On-the-fly syntax checking
(use-package flycheck
  :config (setq truncate-lines nil)
  :hook (prog-mode . flycheck-mode))

;; Project Interaction Library for Emacs
(use-package projectile
  :ensure t
  :bind (("C-c p" . projectile-command-map))
  :config
  (setq projectile-mode-line "Projectile")
  (setq projectile-track-known-projects-automatically nil))

(use-package counsel-projectile
  :ensure t
  :after (projectile)
  :init (counsel-projectile-mode))

;; A Git Porcelain inside Emacs
(use-package magit)

;; Treemacs - a tree layout file explorer for Emacs
(use-package treemacs
  :ensure t
  :defer t
  :config
  (treemacs-tag-follow-mode)
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ;; ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag))
  (:map treemacs-mode-map
	("/" . treemacs-advanced-helpful-hydra)))

(use-package treemacs-projectile
  :ensure t
  :after (treemacs projectile))

;; If you like debugging in Emacs, enable the next lines.
;; I disabled it, as it imports too many dependencies.Such as:
;; posframe,lsp-treemacs(dash, treemacs[hydra(cl-lib), ace-window(avy)])
;; (use-package dap-mode
;;   :init
;;   (add-hook 'lsp-mode-hook 'dap-mode)
;;   (add-hook 'dap-mode-hook 'dap-ui-mode)
;;   (add-hook 'dap-mode-hook 'dap-tooltip-mode)
;;   (add-hook 'python-mode-hook (lambda() (require 'dap-python)))
;;   (add-hook 'go-mode-hook (lambda() (require 'dap-go)))
;;   (add-hook 'java-mode-hook (lambda() (require 'dap-java))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; settings for Program Languages ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Lisp
;; Short and sweet LISP editing
(use-package lispy
  :init (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1))))

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

(use-package quick-run)

(provide 'init-ide)
;;; init-ide.el ends here
