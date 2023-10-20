(setq inhibit-startup-message t ; Don't show the splash screen
      visible-bell nil)         ; Flash when the bell right

;; Turn off some unneeded UI elements
(menu-bar-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Highlight current line
(hl-line-mode 1)
;; Let the cursor blink
(blink-cursor-mode 1)

;; Record recently opened file
(recentf-mode 1)

;; Remember and restore the last cursor location of opened files
(save-place-mode 1)

;; Move customization variables to a separate file and load it
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(when (file-exists-p custom-file)
  (load custom-file 'noerror 'nomessage))

;; Don't pop up UI dialogs when prompting
(setq use-dialog-box nil)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Encoding config
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;; Set garbage colection threshold to improve startup speed
(setq gc-conc-threshold most-positive-fixnum)

(provide 'init-startup)
