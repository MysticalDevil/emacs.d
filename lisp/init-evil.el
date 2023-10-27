;;; init-evil.el --- Evil about config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'ace-window)
(require 'treemacs)

(defun treemacs-is-treemacs-window-selected? ()
  "Check if the treemacs window is selected (has focus)."
  (and (boundp 'treemacs--buffer)
       (eq (current-buffer) treemacs--buffer)))

(defun +private/treemacs-back-and-forth ()
  "Auto close treemacs window."
  (interactive)
  (if (treemacs-is-treemacs-window-selected?)
      (aw-flip-window)
    (treemacs-select-window)))

(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding t
        evil-want-C-u-scroll t
        evil-want-C-i-jump nil)
  :config
  ;; (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-define-key 'normal 'global (kbd "<leader>ts") #'+private/treemacs-back-and-forth)
  (evil-define-key 'normal 'global (kbd "<leader>op") #'treemacs))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-lion
  :config
  (evil-lion-mode))

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package evil-collection
  :after (evil)
  :config (evil-collection-init))

(provide 'init-evil)
;;; init-evil.el ends here
