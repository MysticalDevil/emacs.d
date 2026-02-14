;;; keybinds.el --- Global keybindings -*- lexical-binding: t; -*-

;; Remap common built-ins to Consult equivalents.
(global-set-key [remap switch-to-buffer] #'consult-buffer)
(global-set-key [remap goto-line]        #'consult-goto-line)
(global-set-key [remap yank-pop]         #'consult-yank-pop)
(global-set-key [remap bookmark-jump]    #'consult-bookmark)

;; Search / navigation.
(global-set-key (kbd "C-c k") #'consult-ripgrep)
(global-set-key (kbd "C-c i") #'consult-imenu)
(global-set-key (kbd "C-c s l") #'consult-line)
(global-set-key (kbd "C-c s m") #'consult-mark)
(global-set-key (kbd "C-c s o") #'consult-outline)
(global-set-key (kbd "C-c s g") #'consult-git-grep)

;; Project workflow and file tree.
(global-set-key (kbd "C-c p p") #'project-switch-project)
(global-set-key (kbd "C-c p f") #'project-find-file)
(global-set-key (kbd "C-c p b") #'consult-project-buffer)
(global-set-key (kbd "C-c t t") #'treemacs)
(global-set-key (kbd "C-c t f") #'treemacs-find-file)

;; Embark actions.
(global-set-key (kbd "C-.") #'embark-act)
(global-set-key (kbd "C-;") #'embark-dwim)

(provide 'keybinds)
;;; keybinds.el ends here
