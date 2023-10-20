(setq inhibit-startup-message t ; Don't show the splash screen
      visible-bell nil)         ; Flash when the bell right

;; Turn off some unneeded UI elements
(menu-bar-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-display-line-numbers-mode 1)


;; Load the Modus Vivendi dark theme
(load-theme 'modus-vivendi t)

(hl-line-mode 1)
(blink-cursor-mode 1)

;; Record recently opened file
(recentf-mode 1)

;; Remember and restore the last cursor location of opened files
(save-place-mode 1)

;; Move customization variables to a separate file and load it
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; Don't pop up UI dialogs when prompting
(setq use-dialog-box nil)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)


;; Set package center mirrors
(setq package-archives '(("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
			 ("gnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
			 ("org" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))

(setq package-check-signature nil) ; Don't check signatures
(require 'package)

(unless (bound-and-true-p package--initialized)
  (package-initialize))

(unless package-archive-contents
  (packagerefresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Use use-package to manage extensions
(setq use-package-always-ensure t ; Global ensure keyword
      use-package-always-defer t ; Global defer load keyword
      use-package-always-demand nil
      use-package-expand-minimally t
      use-package-verbose t)

(require 'use-package)

(use-package restart-emacs) ; Restart emacs from within emacs

(use-package atom-one-dark-theme ; Atom one dark theme
  :init (load-theme 'atom-one-dark t))

;; An atom-one-dark theme for smart-mode-line
(use-package smart-mode-line-atom-one-dark-theme)

(use-package smart-mode-line ; A coded smart mode-line
  :init (setq sml/no-confirm-load-theme t
	      sml/theme 'atom-one-dark)
  (sml/setup))
