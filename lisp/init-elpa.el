;; Set package center mirrors
(setq package-archives '(("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
			 ("gnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
			 ("org" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))

(setq package-check-signature nil) ; Don't check signatures
(require 'package)

(unless (bound-and-true-p package--initialized)
  (package-initialize))

(unless package-archive-contents
  (packagerefresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Use use-package to manage extensions
(setq use-package-always-ensure t ; Global ensure keyword
      use-package-always-defer t ; Global defer load keyword
      use-package-always-demand nil
      use-package-expand-minimally t
      use-package-verbose t)

(require 'use-package)

(provide 'init-elpa)
