;; Atom one dark theme
(use-package atom-one-dark-theme
  :init (load-theme 'atom-one-dark t))
;; An atom-one-dark theme for smart-mode-line
(use-package smart-mode-line-atom-one-dark-theme)
;; A coded smart mode-line
(use-package smart-mode-line
  :init (setq sml/no-confirm-load-theme t
	      sml/theme 'atom-one-dark)
  (sml/setup))
;; Make emacs scroll smoothly
(use-package smooth-scrolling
  :init (smooth-scrolling-mode 1))

(provide 'init-ui)
