#!/usr/bin/env bash

echo "Attemping startup..."
${EMACS:=emacs} -nw --batch \
    --eval '(progn
  (defvar url-show-status)
  (let ((debug-on-error t)
        (url-show-status nil)
        (user-emacs-directory default-directory_
        (user-init-file (expand-file-name "init.el"))
        (load-path (delq default-directory load-path)))
      (load-filr user-init-file)
(run-hooks (quote (after-init-hook)))))'
echo "Startup successful"
