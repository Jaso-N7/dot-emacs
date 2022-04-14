;;; package --- Summary
;;; Commentary

;; Use this at the top of your .emacs file for local overrides
;; (let ((init "~/.emacs.d/init.el"))
;;   (if (file-exists-p init)
;;       (load-file init)
;;     (load-file (substring init 0 -1))))

;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; 
;;                        PACKAGE MANAGERS                           ;;
;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)


;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; 
;;;           GENERAL SETTINGS --- Overall Emacs settings           ;;;
;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;;

;; Initial window and default window
(setq inhibit-startup-screen t)
;; Don't use messages that you don't read
(setq initial-scratch-message "")

;; Graphical User interface settings
(if (fboundp 'menu-bar-mode) (menu-bar-mode 0))
(tool-bar-mode -1)
(column-number-mode 1)
(blink-cursor-mode 0)

;; set highlighting brackets
(progn (show-paren-mode 1)
       (setq show-paren-style 'parenthesis))

;; set default font
(when (member "Anka/Coder" (font-family-list))
  (set-frame-font "Anka/Coder-13" t t))

;; Turn off Line Wrap
(set-default 'truncate-lines nil)

;; Turn off Audio Bell
(setq visible-bell t)
;; Display time
(display-time)
;; Always answer 'y' or 'n'
(fset 'yes-or-no-p 'y-or-no-p)
;; Save backups to a central directory
(setq backup-directory-alist `(("." . "~/.emacs.saves")))

(defun show-file-name ()
  "Show the full path file name in the minibuffer"
  (interactive)
  (message (buffer-file-name)))

;;; Display full path to the file in minibuffer
(global-set-key (kbd "<f5>") 'show-file-name)

;; Easily move point between windows using Shift+<Arrow Key>
(windmove-default-keybindings)

;; Encoding
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; display “lambda” as “λ”
(global-prettify-symbols-mode 1)

(defun add-prettify-symbols ()
  "make some word or string show as pretty Unicode symbols"
  (setq prettify-symbols-alist
        '(;; Greek
          ("lambda" . 955)		; λ
	  ("pi" . 960)

	  ;; Assignment
	  ("let" . 8658)                ; ⇒
	  ("let*" . 8594)               ; →
	  ("flet" . 8797)
	  ("defun" . 8614)		; ↦
	  ("funcall" . 119891)
	  ("apply" . 10765)
	  ("setf" . 10566)              ; ⤽ ⥆
	  
	  ;; Identity, Equivalence
	  ("eq" . 8803)
	  ("eql" . 8801)
	  ("equal" . 8781)
	  ("equalp" . 8799)

	  ;; Logic
	  ("not" . 172)
	  ("T" . 8872)
	  ("NIL" . 8877)
	  ("for-all" . 8704)
	  ("complement" . 8705)
	  ("assert" . 8870)
	  ("and" . 8743)
	  ("or" . 8744)
	  ("cond" . 8866)

	  ;; Math Operators
	  ("/" . 247)
	  ("*" . 8901)
	  ("sqrt" . 8730)
	  ("exp" . 8495)

	  ;; Sequences
	  ("'()" . 8709)
	  ("member" . 8715)
	  ("member-if" . 8957)
	  ("member-if-not" . 8716)
	  ("elt" . 8712)
	  ("subset" . 8834)
	  ("union" . 8746)
	  ("intersection" . 8745)
	  ("append" . 10746)        ;
	  ("concatenate" . 10747)   ;
	  ("length" . 10230)        ; 

	  ;; Maps
	  ("map" . 10204)
	  ("mapcar" . 8886)
	  ("maplist" . 8887)
	  ("mapc" . 8888)

	  ;; Order
	  ("<" . 8826)
	  (">" . 8827)
	  ("<=" . 8828)
	  (">=" . 8829)
	  
	  ;; Arrows
          ("->" . 8594)			; →
          ("=>" . 8658)			; ⇒

          )))

;(add-hook 'lisp-mode-hook 'add-prettify-symbols)
(add-hook 'sly-mrepl-mode-hook 'add-prettify-symbols)

;;; ;;; ;;; ;;; ;;; ;;; END GENERAL SETTINGS ;;; ;;; ;;; ;;; ;;; ;;; 

;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; 
;;                            PACKAGES                               ;;
;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;;

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;     LISP --- Everything related to Common Lisp Development       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package sly
  :ensure t
  :config
  (setq sly-lisp-implementations ;; Invoke with `M--M-x sly'
	'((ccl ("wx86cl64"))
	  (cmucl ("/opt/cmucl-20c/bin/lisp" "-quiet"))))
  :bind (:map sly-prefix-map
	      ("M-h" . sly-documentation-lookup)))

(use-package lispy
  :ensure t
  :hook ((emacs-lisp-mode . lispy-mode)
	 (lisp-mode . lispy-mode)
	 (sly-mrepl-mode . lispy-mode))
  :config (lispy-mode 1))

(use-package paren-face
  :ensure t
  :config (global-paren-face-mode 1))

;; Lisp Docker Container Development
(use-package slime-docker
  :ensure t
  :custom
  (slime-docker-program "ccl")
  (slime-docker-ports '((:ip "127.0.0.1"
			     :host-port 8080
			     :container-port 8080)))
  (slime-docker-env '(("QUICKLISP_ADD_TO_INIT_FILE" . "true")))
  (slime-docker-mounts `(((,(expand-file-name "~/src/") . "/home/cl/quicklisp/local-projects/")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;            HTML/CSS/JS --- Web Development packages              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package web-mode
  :ensure t
  :mode "\\.html?\\'" "\\.js\\'" "\\.css\\'"
  "\\.clp\\'"				; Common Lisp Server Pages
  :custom
  (web-mode-engines-alist '(("clp" . "\\.clp\\'")))
  (web-mode-ac-sources-alist
	'(("css" . (ac-source-css-property))
	  ("js" . (ac-source-css-property))
	  ("html" . (ac-source-words-in-buffer ac-source-abbrev))
	  ("clp" . (ac-source-words-in-buffer ac-source-abbrev))))
  (web-mode-enable-auto-closing t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;        MISC --- Convenience and other personal preferences       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package golden-ratio
  :ensure t
  :config (golden-ratio-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;           COLOUR THEMES --- Look and feel of the editor          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; M-x load-theme 
;; OR C-x C-e at the end of the theme settings

					;(use-package material-theme
					;  :ensure t
					;  :config (load-theme 'material-light t))

;;; Alternate themes I have enjoyed and will switch to on occasion:
;; - DARK -
;; NORD - https://github.com/arcticicestudio/nord-emacs
					;(use-package nord-theme
					;  :ensure nil
					;  :config (load-theme 'nord t)
					;  (setq nord-region-highlight "snowstorm"))

;; TRON
					;(use-package tron-legacy-theme
					;  :ensure nil
					;  :config 
					;  (setq tron-legacy-theme-vivid-cursor t)
					;  (load-theme 'tron-legacy t))

;; SOLARIZED
;(use-package solarized-theme
;  :ensure t
;  :config
;  (load-theme 'solarized-light t)
;  (setq solarized-high-contrast-mode-line t))

;; - LIGHT -
;; FLATUI
;; SOLARIZED

;;; ;;; ;;; ;;; ;;; ;;; ;;; END PACKAGES  ;;; ;;; ;;; ;;; ;;; ;;; 

;; This allows customizations to be written to the "~/.emacs" file
(provide '.emacs)
;;; .emacs ends here
