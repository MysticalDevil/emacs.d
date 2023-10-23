;;; init-ui.el -- settings for ui

;;; Commentary:
;;; Code:

(toggle-frame-maximized)

;; Atom one dark theme
(use-package atom-one-dark-theme
  :init (load-theme 'atom-one-dark t))

;; An atom-one-dark theme for smart-mode-line
(use-package smart-mode-line-atom-one-dark-theme)

;; A coded smart mode-line
(use-package smart-mode-line
  :init (setq sml/no-confirm-load-theme t
              sml/theme 'atom-one-dark)
  (sml/setup)
  :config
  (setq rm-blacklist
	(format "^ \\(%s\\)$"
		(mapconcat #'identity
			   '("Projectile.*" "company.*" "Google"
			     "Undo-Tree" "counsel" "ivy" "yas" "WK")
			   "\\|"))))

;; Make emacs scroll smoothly
(use-package smooth-scrolling
  :init (smooth-scrolling-mode 1))

;; Attempt at good pixel-based smooth scrolling in Emacs
(use-package good-scroll
  :init (good-scroll-mode 1))

;; A company front-end with icons
(use-package company-box
  :hook (company-mode . company-box-mode))

;; An extensible emacs dashboard
(use-package dashboard
  :init
  (dashboard-open)
  (add-hook 'find-file-hook
	    (lambda ()
	      (when (string= (file-name-extension buffer-file-name) "*dashboard*")
		(line-number-mode -1))))
  :config
  (dashboard-setup-startup-hook)
  ;; Set the title
  (setq dashboard-banner-logo-title "Welcome to Emacs Dashboard")
  ;; Set the banner
  (setq dashboard-startup-banner [VALUE])
  ;; Value can be
  ;; - nil to display no banner
  ;; - 'official which displays the official emacs logo
  ;; - 'logo which displays an alternative emacs logo
  ;; - 1, 2 or 3 which displays one of the text banners
  ;; - "path/to/your/image.gif", "path/to/your/image.png" or "path/to/your/text.txt" which displays whatever gif/image/text you would prefer
  ;; - a cons of '("path/to/your/image.png" . "path/to/your/text.txt")

  ;; Content is not centered by default. To center, set
  (setq dashboard-center-content t)

  ;; To disable shortcut "jump" indicators for each section, set
  (setq dashboard-show-shortcuts nil)

  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5)
                          (registers . 5))))



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
