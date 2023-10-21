;;; init-kbd.el -- Keybindings settiings

;;; Commentary:
;;; Code:

(when *is-mac*
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none))

(provide 'init-kbd)
;;; init-kbd.el ends here
