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
  ;; Invoke with `M--M-x sly'
  :init (setq sly-lisp-implementations
	      '((ccl "ccl")
		(cmucl ("/opt/cmucl-20c/bin/lisp" "-quiet"))))
  :config (setq inferior-lisp-program "ccl"))

