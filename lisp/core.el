;;; core.el --- Package/bootstrap core -*- lexical-binding: t; -*-

;; Prefer newer source files over stale byte-compiled artifacts.
(setq load-prefer-newer t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory))
      (bootstrap-version 7))
  ;; Keep startup alive when network/bootstrap is unavailable.
  (condition-case err
      (progn
        (unless (file-exists-p bootstrap-file)
          (with-current-buffer
              (url-retrieve-synchronously
               "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
               'silent 'inhibit-cookies)
            (goto-char (point-max))
            (eval-print-last-sexp)))
        (load bootstrap-file nil 'nomessage)
        (setq straight-use-package-by-default t
              use-package-expand-minimally t))
    (error
     (display-warning
      'core
      (format "straight bootstrap unavailable: %s" (error-message-string err))
      :warning))))

;; Prefer real use-package when available; fallback to a harmless no-op macro.
(unless (require 'use-package nil 'noerror)
  (display-warning 'core "use-package not found; package declarations are skipped" :warning)
  (defmacro use-package (&rest _args)
    "Fallback no-op when `use-package' is unavailable."
    nil))

(provide 'core)
;;; core.el ends here
