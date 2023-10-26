;;; init-system.el --- configs for startup -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq auto-save-default nil
      auto-window-vscroll nil
      delete-by-moving-to-trash t
      fast-but-imprecise-scrolling t
      frame-title-format "%b"
      help-window-select t
      inhibit-startup-screen t
      inhibit-default-init t
      ;; initial-scratch-message nil
      inhibit-compacting-font-caches t
      initial-major-mode 'fundamental-mode
      make-backup-files nil             ; disable backup file
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse t
      next-line-add-newlines nil
      read-process-output-max (* 64 1024)
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position t
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      visible-bell nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; =====================OS Specific==================== ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Windows
;; spcial coding settings for Windows
(unless (memq system-type '(cygwin windows-nt ms-dos))
  (setq selection-coding-system 'utf-8))

;; Set font size
(set-frame-font "MesloLGS Nerd Font 12" nil t)

(provide 'init-system)
;;; init-system.el ends here
