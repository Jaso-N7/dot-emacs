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
	      '((ccl ("ccl"))
		(cmucl ("/opt/cmucl-20c/bin/lisp" "-quiet"))))
  :config (setq inferior-lisp-program "ccl"))

;;; THEMES
;; M-x load-theme 
;; OR C-x C-e at the end of the theme settings

(use-package material-theme
  :ensure t
  :config (load-theme 'material-light t))

;;; Alternate themes I have enjoyed and will switch to on occasion:
;; - DARK -
;; NORD - https://github.com/arcticicestudio/nord-emacs
(use-package nord-theme
  :ensure nil
  :config (load-theme 'nord t)
  :init (setq nord-region-highlight "snowstorm"))

;; TRON
(use-package tron-legacy-theme
  :ensure nil
  :config 
  (setq tron-legacy-theme-vivid-cursor t)
  (load-theme 'tron-legacy t))

;; SOLARIZED
(use-package solarized
  :ensure nil
  :init (setq solarized-high-contrast-mode-line t)
  :config (load-theme 'solarized-dark t))

;; - LIGHT -
;; SOLARIZED
(use-package solarized
  :ensure nil
  :init (setq solarized-high-contrast-mode-line t)
  :config (load-theme 'solarized-light t))



(use-package lispy
  :ensure t
  :hook lisp-mode 
  :config (lispy-mode 1))
