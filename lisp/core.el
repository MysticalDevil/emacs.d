;;; early-init.el --- Early init for Emacs 30+ -*- lexical-binding: t; -*-

;; Disable package.el auto-init; straight/use-package will handle packages.
(setq package-enable-at-startup nil)

;; ----- GC tuning during startup -----
(defconst my/gc-cons-threshold-startup (* 128 1024 1024) "Startup GC threshold (bytes).")
(defconst my/gc-cons-threshold-normal  (* 16  1024 1024) "Normal GC threshold (bytes).")

(setq gc-cons-threshold my/gc-cons-threshold-startup
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold my/gc-cons-threshold-normal
                  gc-cons-percentage 0.1)))

;; ----- Reduce startup overhead from file handlers -----
(defvar my/file-name-handler-alist-original file-name-handler-alist
  "Original `file-name-handler-alist' saved during startup.")
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist my/file-name-handler-alist-original)))

;; ----- Native compilation warnings -----
(when (boundp 'native-comp-async-report-warnings-errors)
  (setq native-comp-async-report-warnings-errors 'silent))

;; ----- Improve subprocess throughput (LSP, ripgrep, git, etc.) -----
(setq read-process-output-max (* 1024 1024)) ; 1MiB
(when (boundp 'process-adaptive-read-buffering)
  (setq process-adaptive-read-buffering nil))

;; ----- Rendering / font caches -----
(when (boundp 'inhibit-compacting-font-caches)
  (setq inhibit-compacting-font-caches t))
(when (boundp 'frame-inhibit-implied-resize)
  (setq frame-inhibit-implied-resize t))

;; ----- Frame defaults (avoid first-frame flicker) -----
(setq frame-resize-pixelwise t)

(push '(fullscreen . maximized) default-frame-alist)
(push '(internal-border-width . 8) default-frame-alist)
(push '(left-fringe . 8) default-frame-alist)
(push '(right-fringe . 8) default-frame-alist)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
(push '(horizontal-scroll-bars . nil) default-frame-alist)

;; Disable UI chrome as early as possible (safe in TTY/daemon).
(when (fboundp 'menu-bar-mode)   (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Keep startup cleaner; warnings are still accessible if needed.
(setq warning-minimum-level :error)

(provide 'early-init)
;;; early-init.el ends here
