;; Restart emacs from within emacs
(use-package restart-emacs)

;; Benchmarks for require and load calls
(use-package benchmark-init
  :init (benchmark-init/activate)
  :hook (after-init . benchmark-init/deactivate))

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

;; Use y_n to replace yes_no
(use-package emacs
  :config (defalias 'yes-or-no-p 'y-or-n-p))

(provide 'init-package)
