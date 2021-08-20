(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package sly
  :ensure t
  :config (setq inferior-lisp-program "ccl"))

