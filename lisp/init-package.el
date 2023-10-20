;; Restart emacs from within emacs
(use-package restart-emacs)

;; Benchmarks for require and load calls
(use-package benchmark-init
  :init (benchmark-init/activate)
  :hook (after-init . benchmark-init/deactivate))

;; Use y_n to replace yes_no
(use-package emacs
  :config (defalias 'yes-or-no-p 'y-or-n-p))

(provide 'init-package)
