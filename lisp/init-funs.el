;;; init-funs.el --- Define functions.	-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun icons-displayable-p ()
  "Retuen non-nil if icons are displayable."
  (or (featurep 'nerd-icons)
      (require 'nerd-icons nil t)))

(provide 'init-funs)
;;; init-funs.el ends here
