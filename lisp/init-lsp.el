;;; init-lsp.el -- LSP about settings

;;; Commentary:
;;; Code:

(use-package lsp-mode
  ;; add prog=mode to lsp insted of adding one by one
  ;; :hook (prog-mode . (lsp-deferred))
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :commands (lsp lsp-deferred lsp-format-buffer lsp-organize-imports)
  :init
  (add-hook 'lsp-mode-hook (lambda ()
			     (add-hook 'before-save-hook #'lsp-organize-imports t t)
			     (add-hook 'before-save-hook #'lsp-format-buffer t t)))
  (add-hook 'prog-mode (lambda ()
			 (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode)(lsp-deferred))))
  :config
  (setq lsp-auto-guess-root t
	lsp-headerline-breadcrumb-enable nil)
  (setq lsp-keymap-prefix "C-c l")
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map))

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :init
  (setq lsp-ui-doc-include-signature t
	lsp-ui-doc-position 'at-point
	lsp-ui-sideline-ignore-duplicate t)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (add-hook 'lsp-ui-mode-hook 'lsp-modeline-code-actions-mode)
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definations] #'lsp-ui-peek-find-definations)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

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

(provide 'init-lsp)
;;; init-lsp.el ends here
