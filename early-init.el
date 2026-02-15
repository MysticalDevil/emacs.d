;;; early-init.el --- Early init for Emacs 30+ -*- lexical-binding: t; -*-

;; Disable the built-in package.el auto-initialization
(setq package-enable-at-startup nil)

;; Minimize GC interference during the startup phase; resume after startup is complete.
(defconst my/gc-cons-threshold-startup (* 128 1024 1024)) ; 128MiB
(defconst my/gc-cons-threshold-normal  (* 16  1024 1024)) ; 16MiB

(setq gc-cons-threshold my/gc-cons-threshold-startup
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda()
            (setq gc-cons-threshold my/gc-cons-threshold-normal
                  gc-cons-percentage 0.1)))

;; file-name-handler-alist slows down require/load during startup; recovers after startup
(defvar my/file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda()
            (setq file-name-handler-alist my/file-name-handler-alist-original)))

;; Native Compilation: Reduce disruptive alerts (enable stricter settings as needed)
(when (boundp 'native-comp-async-report-warnings-errors)
  (setq native-comp-async-report-warnings-errors 'silent))

;; I/O/Subprocess Throughput (Useful for LSP, rg, git, etc.)
(setq read-process-output-max (* 1024 1024)) ; 1MiB
(when (boundp 'process-adaptive-read-buffering)
  (setq process-adaptive-read-buffering nil))

;; Font Caching and Frame Resize: Reducing Startup Jitter
(when (boundp 'inhibit-compacting-font-caches)
  (setq inhibit-compacting-font-caches t))
(when (boundp 'frame-inhibit-implied-resize)
  (setq frame-inhibit-implied-resize t))

;; A cleaner default UI (applies in GUI mode)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
(push '(horizontal-scroll-bars . nil) default-frame-alist)
(push '(internal-border-width . 9) default-frame-alist)
(push '(left-fringe . 8) default-frame-alist)
(push '(right-fringe . 8) default-frame-alist)
(setq frame-resize-pixelwise t)
(setq-default cursor-in-non-selected-windows nil)

(when (fboundp 'menu-bar-mode)   (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq warning-minimum-level :warning)

(provide 'early-init)
;;; early-init.el ends here
