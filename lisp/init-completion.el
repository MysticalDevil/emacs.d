;;; init-completion.el --- Completions settings, include minibuffers, buffers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

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
  :bind (("M-x" . counsel-M-x))
  ("C-x C-f" . counsel-find-file)
  ("C-c r" . counsel-recentf)
  ("C-c g" . counsel-git))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper))
  ("C-r" . swiper-isearch)
  :config (setq swiper-action-recenter t
                swiper-include-line-number-in-search t))

(use-package ivy-rich
  :hook (ivy-mode . ivy-rich-mode)
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

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
(use-package company-box
  :hook (company-mode . company-box-mode))

;; Languages backend
(use-package company-go
  :init
  (add-hook 'go-mode-hook (lambda ()
                            (set (make-local-variable 'company-backends) '(company-go))
                            (company-mode)))
  :after (company))

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

(provide 'init-completion)
;;; init-completion.el ends here
