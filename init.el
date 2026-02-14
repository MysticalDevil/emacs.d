;;; init.el -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'core)
(require 'ui)
(require 'packages)
(require 'keybinds)
;; (require 'editing)
;; (require 'langs)

(provide 'init)
