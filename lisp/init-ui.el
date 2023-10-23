;;; init-ui.el -- settings for ui

;;; Commentary:
;;; Code:

(toggle-frame-maximized)

(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  (doom-themes-treemacs-theme "doom-vscode")
  :config
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

;; A fancy and fast mode-line inspired by minimalism design.
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-support-imenu t)
  :custom-face
  (doom-modeline ((t (:family "MesloLGS Nerd Font" :height 100))))
  (doom-modeline-inactive ((t (:family "MesloLGS Nerd Font" :height 100))))
  (doom-modeline-battery-full ((t (:inherit success :weight extra-bold))))
  :config
  (setq inhibit-compacting-font-caches t
        doom-modeline-minor-modes t
        doom-modeline-height 24
        doom-modeline-bar-width 4
        doom-modeline-enable-word-count t
        doom-modeline-indent-info t
        doom-modeline-set-pdf-modeline t
        doom-modeline-github t
        doom-modeline-buffer-file-name-style 'file
        doom-modeline-project-detection 'Ffip))

;; A minor-mode menu for the mode line
(use-package minions
  :hook (doom-modeline-mode . minions-mode))

;; Emacs Port of anzu.vim
(use-package anzu
  :custom
  (anzu-mode-lighter "")
  (anzu-deactivate-region t)
  (anzu-search-threshold 1000)
  (anzu-replace-threshold 50)
  (anzu-replace-to-string-separator " => ")
  :config
  (global-anzu-mode +1)
  (define-key isearch-mode-map [remap isearch-query-replace]  #'anzu-isearch-query-replace)
  (define-key isearch-mode-map [remap isearch-query-replace-regexp] #'anzu-isearch-query-replace-regexp))

;; Make emacs scroll smoothly
(use-package smooth-scrolling
  :init (smooth-scrolling-mode 1))

;; Attempt at good pixel-based smooth scrolling in Emacs
(use-package good-scroll
  :init (good-scroll-mode 1))

;; Emacs plugin aiming to become an aesthetic, modern looking tabs plugin
(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-headline-match)
  :custom
  (centaur-tabs-style "bar")
  (centuar-tabs-height 48)
  (centaur-tabs-set-icons t)
  (centaur-tabs-plain-icons t)
  (centaur-tabs-show-navigation-buttons t)
  (centaur-tabs-set-bar 'under)
  (x-unferline-at-decent-lint t)
  (centaur-tabs-gray-out-icons 'buffer)
  (centaur-tabs-sycle-scope 'tabs)
  (centaur-tabs-set-modified-marker t)
  :bind (("C-<prior>" . centaur-tabs-backward)
         ("C-<next>" . centaur-tabs-forward))
  :hook (dired-mode . centaur-tabs-local-mode))

;; An extensible emacs dashboard
(use-package dashboard
  :init
  ;; Format: "(icon title help action face prefix suffix)"
  (setq dashboard-navigator-buttons `(((,(if (fboundp 'all-the-icons-octicon) (all-the-icons-octicon "mark-github" :height 1.0 :v-adjust 0.0) "★")
                                        "GitHub" "Browse" (lambda (&rest _) (browse-url homepage-url))))))

  :hook ((after-init . dashboard-setup-startup-hook)
         (dashboard-mode . (lambda ()
                             (setq-local global-hl-line-mode nil))))
  :config
  (defconst homepage-url "https://github.com/MysticalDevil/emacs.d")
  :custom
  (dashboard-projects-backend 'projectile)
  (dashboard-banner-logo-title "This is a devil")
  (dashboard-startup-banner "~/.config/emacs/img/logo.svg")
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-init-info t)
  (dashboard-set-navigator t)
  (dashboard-items '((recents . 5)
                     (projects . 5)
                     (bookmarks . 5))))



;; Change font on windows to reduce lag
(use-package emacs
  :if (display-graphic-p)
  :config
  ;; Font settings
  (if *is-windows*
      (progn
        (set-face-attribute 'default nil :font "Microsoft Yahei Mono 9")
        (dolist (charset '(kana han symbol cjk-misc bopomofo))
          (set-fontset-font (frame-parameter nil 'font)
                            charset (font-spec :family "Microsoft Yahei Mono" :size 12))))
    (set-face-attribute 'default nil :font "MesloLGS Nerd Font")))

(provide 'init-ui)
;;; init-ui.el ends here
