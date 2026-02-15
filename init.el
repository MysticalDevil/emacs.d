;;; init.el --- Entry point for modular Emacs config -*- lexical-binding: t; -*-

;;; Commentary:
;; Top-level entry that loads feature modules from `lisp/' in a fixed order.

;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(defvar my/init-modules
  ;; Loading order: bootstrap -> UI -> package/keymaps -> editing/langs.
  '(core ui packages keybinds editing langs)
  "Ordered list of feature modules to load from `lisp/'.")

(defun my/require-module (feature)
  "Require FEATURE and warn instead of aborting startup on failure."
  (condition-case err
      (require feature)
    (error
     (display-warning
      'init
      (format "Failed loading module `%s`: %s" feature (error-message-string err))
      :warning)
     nil)))

(dolist (feature my/init-modules)
  (my/require-module feature))

(provide 'init)
;;; init.el ends here
