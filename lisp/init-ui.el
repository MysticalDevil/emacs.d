;;; init-ui.el -- settings for ui

;;; Commentary:
;;; Code:

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

;; A utility package to collect various Icon Fonts and propertize them within Emacs.
(use-package all-the-icons
  :if (display-graphic-p))

;; A company front-end with icons
(use-package company-box
  :hook (company-mode . company-box-mode))

;; An extensible emacs dashboard
(use-package dashboard
  :init
  (dashboard-open)
  (add-hook 'find-file-hook
	    (lambda ()
	      (when (string= (file-name-extension buffer-file-name) "dashboard")
		(line-number-mode -1))))
  :config
  (dashboard-setup-startup-hook))

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

;; Show line number
(use-package emacs
  :config
  (setq display-line-numbers-type 'absolute)
  (global-display-line-numbers-mode 1))

(provide 'init-ui)
;;; init-ui.el ends here
