;;; ui.el --- UI setup (fonts/line numbers/theme/faces) -*- lexical-binding: t; -*-

;;; Commentary:
;; Visual/UI configuration: fonts, theme, modeline, dashboard, and indicators.

;;; Code:

(require 'cl-lib)
(declare-function doom-themes-visual-bell-config "doom-themes")
(declare-function doom-themes-org-config "doom-themes")

;; --------------------
;; User-tunable knobs
;; --------------------
(defvar my/font-size 16
  "Default font size in points.")

(defvar my/font-mono-candidates
  '("JetBrains Mono" "Iosevka" "Fira Code" "Cascadia Mono" "Source Code Pro" "Monospace")
  "Preferred mono fonts; first available wins.")

(defvar my/font-cjk-candidates
  '("Noto Sans CJK SC" "Source Han Sans SC" "WenQuanYi Micro Hei" "PingFang SC")
  "Preferred CJK fonts; first available wins.")

(defvar my/font-ui-candidates
  '("Noto Sans" "Source Han Sans SC" "WenQuanYi Micro Hei" "Sans Serif")
  "Preferred variable-pitch UI fonts; first available wins.")

(defvar my/theme 'doom-tokyo-night
  "Theme symbol, defaulting to Doom theme `doom-tokyo-night'.")

(defvar my/line-numbers-style 'relative
  "Line number style: t | `relative' | nil.")

;; --------------------
;; Helpers
;; --------------------
(defun my/font-available-p (name)
  "Return non-nil if font family NAME is available."
  (and (stringp name) (find-font (font-spec :family name))))

(defun my/first-available-font (candidates)
  "Pick the first available font from CANDIDATES."
  (cl-find-if #'my/font-available-p candidates))

(defun my/apply-fonts (&optional frame)
  "Apply fonts to FRAME (or the selected frame). GUI only."
  (let ((f (or frame (selected-frame))))
    (when (display-graphic-p f)
      (with-selected-frame f
        (when-let ((mono (my/first-available-font my/font-mono-candidates)))
          (set-face-attribute 'default f :family mono :height (* my/font-size 10))
          (set-face-attribute 'fixed-pitch f :family mono))
        ;; Use a dedicated variable-pitch font for help/start buffers.
        (when-let ((ui (my/first-available-font my/font-ui-candidates)))
          (set-face-attribute 'variable-pitch f :family ui :height (* my/font-size 10)))
        ;; Optional CJK fallback (only if you have CJK fonts installed)
        (when-let ((cjk (my/first-available-font my/font-cjk-candidates)))
          (dolist (charset '(han kana cjk-misc bopomofo))
            (set-fontset-font t charset (font-spec :family cjk) nil 'append)))))))

(defun my/apply-faces (&optional frame)
  "Apply face tweaks. If FRAME is non-nil, apply to that frame where applicable."
  ;; Most faces are safe to set globally; frame argument is used where it matters.
  (set-face-attribute 'default (or frame nil) :weight 'normal)
  (set-face-attribute 'mode-line (or frame nil) :box nil)
  (set-face-attribute 'mode-line-inactive (or frame nil) :box nil))

(defun my/apply-theme ()
  "Load `my/theme' and then re-apply face tweaks."
  (mapc #'disable-theme custom-enabled-themes)
  (condition-case nil
      (load-theme my/theme t)
    (error
     (load-theme 'wombat t)))
  (my/apply-faces))

(defun my/apply-ui (&optional frame)
  "Apply per-frame UI bits (fonts + faces) to FRAME."
  (my/apply-fonts frame)
  (my/apply-faces frame))

;; --------------------
;; Basic UI behaviors
;; --------------------
(setq inhibit-startup-screen t
      visible-bell nil
      ring-bell-function 'ignore)

;; Scrolling / rendering
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))
(setq scroll-margin 3
      scroll-conservatively 101
      scroll-step 1)

;; Indicators
(column-number-mode 1)
(size-indication-mode 1)
(global-hl-line-mode 1)
(show-paren-mode 1)

;; Line numbers: enable in prog/text, disable in some modes.
(setq display-line-numbers-type my/line-numbers-style)
(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook #'display-line-numbers-mode))
(dolist (mode '(term-mode eshell-mode shell-mode vterm-mode
                org-mode markdown-mode
                treemacs-mode dired-mode))
  (add-hook (intern (format "%s-hook" mode))
            (lambda () (display-line-numbers-mode -1))))

;; --------------------
;; Theme (third-party: doom-themes) + post-theme face stabilization
;; --------------------
(use-package doom-themes
  :config
  ;; Improves modeline and org face integration for Doom themes.
  (when (fboundp 'doom-themes-visual-bell-config)
    (doom-themes-visual-bell-config))
  (when (fboundp 'doom-themes-org-config)
    (doom-themes-org-config)))

;; --------------------
;; Startup dashboard
;; --------------------
(use-package dashboard
  :init
  ;; Open a dedicated start screen with practical entry points.
  (setq dashboard-startup-banner 'logo
        dashboard-center-content t
        dashboard-show-shortcuts nil
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-items '((recents  . 8)
                          (projects . 5)
                          (agenda   . 5)))
  :config
  (setq dashboard-banner-logo-title "Welcome back to Emacs")
  (dashboard-setup-startup-hook))

;; --------------------
;; Statusline (modeline)
;; --------------------
(use-package nerd-icons
  :defer t)

(use-package doom-modeline
  :init
  (setq doom-modeline-height 28
        doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-buffer-file-name-style 'truncate-with-project
        doom-modeline-minor-modes nil)
  :config
  (doom-modeline-mode 1))

;; Ensure face tweaks persist across theme changes.
(when (boundp 'after-load-theme-hook)
  (add-hook 'after-load-theme-hook #'my/apply-faces))

;; Load theme once (global), then apply per-frame UI for current frame.
(my/apply-theme)
(my/apply-ui)

;; Daemon/new frames: apply fonts/faces to the new frame only (avoid re-loading theme).
(add-hook 'after-make-frame-functions #'my/apply-ui)

(provide 'ui)
;;; ui.el ends here
