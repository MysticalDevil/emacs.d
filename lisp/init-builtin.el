;;; init-builtin.el --- initialize the builtin plugins -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defalias 'yes-or-no-p 'y-or-n-p)

;; Abbr
(setq-default abbrev-mode t)

;; Auto save
;; `save-some-buffers' is provided by files.el (builtin)
;; `pulse-momentary-highlight-one-line' is provided by pulse.el (builtin)
(use-package emacs
  :ensure nil
  :init
  (defun pulse-save-buffers (&rest args)
    (save-some-buffers t)
    (pulse-momentary-highlight-one-line (point)))
  ;; auto save when frame lose focus, Alt-Tab
  (add-function :after after-focus-change-function #'pulse-save-buffers)
  ;; auto save when buffer changed
  (dolist (command '(other-window
                     switch-to-buffer
                     next-buffer
                     previous-buffer))
    (advice-add command :after #'pulse-save-buffers)))

;; Auto revert
;; `global-auto-revert-mode` is provided by autorevert.el (builtin)
(use-package autorevert
  :hook (after-init . global-auto-revert-mode))

;; Delete behavior
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(add-hook 'after-init-hook 'delete-selection-mode)

;; HideShow minor mode
(use-package hideshow
  :init (add-hook 'hs-minor-mode-hook (lambda () (diminish 'hs-minor-mode)))
  :hook (prog-mode . hs-minor-mode))

;; ibuffer
(use-package ibuffer
  :init (defalias 'list-buffers 'ibuffers))

;; Line number
;; this package intruducted in Emacs 26, so only enabled when 26+
(use-package display-line-numbers
  :if (> emacs-major-version 26)
  :hook (prog-mode . display-line-numbers-mode))

;; Org mode
(setq org-hide-leading-stars t
      org-startup-indented t)

;; Parentheses
(use-package paren
  :ensure nil
  :config (setq-default show-paren-style 'mixed
                        show-paren-when-point-inside-paren t
                        show-paren-when-point-in-periphery t)
  :hook (prog-mode . show-paren-mode))

;; Recent files
(add-hook 'after-init-hook (lambda ()
                             (recentf-mode 1)
                             (add-to-list 'recentf-exclude '("~\/.config\/emacs\/elpa\/"))))
(setq-default recentf-max-menu-items 20
              recentf-max-saved-items 20)

;; Save place
(save-place-mode 1)

;; Only use spaces instead of TAB, use C-q TAB to input TAB char
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Diminish builtins
(dolist (elem '(abbrev-mode eldoc-mode))
  (diminish elem))
(add-hook 'hs-minor-mode-hook (lambda () (diminish 'hs-minor-mode)))

;; Prettify symbols mode
(use-package emacs
  :hook (prog-mode . prettify-symbols-mode))

(provide 'init-builtin)
;;; init-builtin.el ends here
