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
(add-to-list 'package-archives
	     '("melpa-stable" . "https://melpa.org/packages/") t)

(package-initialize)


;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; 
;;;           GENERAL SETTINGS --- Overall Emacs settings           ;;;
;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;;

;;; VISUALS ---

;; Initial window and default window
(setq inhibit-startup-screen t)
;; Don't use messages that you don't read
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message
      "Welcome, Jason! What are we creating today?")

;; Graphical User interface settings
(if (fboundp 'menu-bar-mode) (menu-bar-mode 0))
(tool-bar-mode -1)
(column-number-mode 1)
(blink-cursor-mode 0)
 ;; Who uses the bar to scroll?
 (scroll-bar-mode 0)

;; set highlighting brackets
(progn (show-paren-mode 1)
       (setq show-paren-style 'parenthesis))

;; set default font
(when (member "IBM Plex Mono" (font-family-list))
  (set-frame-font "IBM Plex Mono-13" t t))

;; Turn off Line Wrap
(set-default 'truncate-lines nil)

;; Turn off Audio Bell
(setq visible-bell t)
;; Display time
(display-time)
;; Save backups to a central directory
(setq backup-directory-alist `(("." . "~/.emacs.saves")))

;; See http://bzg.fr/emacs-hide-mode-line.html
(defvar-local hidden-mode-line-mode nil)
(defvar-local hide-mode-line nil)

(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global nil
  :variable hidden-mode-line-mode
  :group 'editing-basics
  (if hidden-mode-line-mode
      (setq hide-mode-line mode-line-format
            mode-line-format nil)
    (setq mode-line-format hide-mode-line
          hide-mode-line nil))
  (force-mode-line-update)
  ;; Apparently force-mode-line-update is not always enough to
  ;; redisplay the mode-line
  (redraw-display)
  (when (and (called-interactively-p 'interactive)
             hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "Hidden Mode Line Mode enabled.  "
             "Use M-x hidden-mode-line-mode to make the mode-line appear."))))

;; Activate hidden-mode-line-mode
(hidden-mode-line-mode 1)

;; If you want to hide the mode-line in all new buffers
(add-hook 'after-change-major-mode-hook 'hidden-mode-line-mode)

;; Command to toggle the display of the mode-line as a header
(defvar-local header-line-format nil)
(defun mode-line-in-header ()
  (interactive)
  (if (not header-line-format)
      (setq header-line-format mode-line-format
    	    mode-line-format nil)
    (setq mode-line-format header-line-format
    	  header-line-format nil))
  (set-window-buffer nil (current-buffer)))
(global-set-key (kbd "<f1>") 'mode-line-in-header)


;;; --- END VISUALS

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

(use-package xah-fly-keys
  :preface (setq xah-fly-use-meta-key nil)
  :custom (xah-fly-command-mode-indicator
	   (propertize " ⦿ " 'face '(:foreground "red")))
  :config
  (xah-fly-keys-set-layout "koy")
  ;; make esc key do cancel. works only in gui emacs
  (define-key key-translation-map (kbd "<escape>") (kbd "C-g"))
  
  (xah-fly-keys 1)
  ;; adding a leader key map to clojure mode
  (when (fboundp 'clojure-mode)

    (defun xah-config-clojure-mode ()
      "config clojure-mode. Version 2024-04-04"
      (interactive)
      (progn
	;; create a keymap
	(define-prefix-command 'xah-clojure-leader-map)
	;; add keys to it
	(define-key xah-clojure-leader-map (kbd "e")
 		    'cider-eval-last-sexp)
	(define-key xah-clojure-leader-map (kbd "k")
 		    'cider-load-buffer-and-switch-to-repl-buffer)
	(define-key xah-clojure-leader-map (kbd "q")
 		    'cider-quit)
	;; add more of the major mode key/command here
	)
      ;; modify the major mode's key map, so that a key becomes your leader key
      ;; F10 - compile the file
      ;; C-F10 - specify compiler
      (define-key clojure-mode-map (kbd "<f6>") xah-clojure-leader-map))

    (add-hook 'clojure-mode-hook 'xah-config-clojure-mode)))

(use-package nix-mode
  :disabled t
  :mode "\\.nix\\")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;     CLOJURE --- Everything related to Lisp Development       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package clojure-mode :defer t  ;; :pin melpa-stable
  :config (use-package cider  ;; :pin melpa-stable
	    :ensure t
	    :defer t
	    :hook  ((cider-repl-mode
		    cider-mode)
		   . company-mode)
	    :config (setq cider-repl-use-pretty-printing t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;     LISP --- Everything related to Common Lisp Development       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Allegro Integration
;; (load "/usr/local/acl/eli/fi-site-init.el")

(use-package sly
  :ensure t
  :config 				; "Invoke with `M--M-x sly'"
  (setq sly-lisp-implementations
	'((abcl ("abcl"))
	  (acl ("alisp"))
	  (ccl ("ccl"))
	  (cmucl ("/opt/cmucl-20c/bin/lisp" "-quiet"))))
  :commands sly-prefix-map
  :bind ("M-h" . sly-documentation-lookup))

(use-package lispy :pin melpa-stable
  :ensure t
  :init (setq lispy-compat '(cider))
  :hook ((emacs-lisp-mode
	  lisp-mode
	  sly-mrepl-mode
	  clojure-mode
	  cider-repl-mode)
	 . lispy-mode)
  :config (lispy-mode 1))

(use-package paren-face
  :ensure t
  :config (global-paren-face-mode 1))

;; Lisp Docker Container Development
;; (use-package slime-docker
  ;; :ensure t
  ;; :custom
  ;; (slime-docker-program "ccl")
  ;; (slime-docker-ports '((:ip "127.0.0.1"
			     ;; :host-port 8080
			     ;; :container-port 8080)))
  ;; (slime-docker-env '(("QUICKLISP_ADD_TO_INIT_FILE" . "true")))
  ;; (slime-docker-mounts `(((,(expand-file-name "~/src/") . "/home/cl/quicklisp/local-projects/")))))
  
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

(use-package company :pin melpa-stable
  :ensure t
  :config (global-company-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;           COLOUR THEMES --- Look and feel of the editor          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; M-x load-theme 
;; OR C-x C-e at the end of the theme settings

;; NANO
(use-package nano-theme
  :disabled t
  :config
  (setq nano-font-family-monospaced "Roboto Mono")
  (setq nano-font-family-proportional nil)
  (setq nano-font-size 16)
  (load-theme 'nano-light nil))

(use-package material-theme
  :disabled t
  :config (load-theme 'material-light t))

;;; Alternate themes I have enjoyed and will switch to on occasion:
;; - DARK -
;; NORD - https://github.com/arcticicestudio/nord-emacs
(use-package nord-theme
  :disabled t
  :config (load-theme 'nord t)
  :custom (nord-region-highlight "snowstorm"))

;; TRON
(use-package tron-legacy-theme
  :config (load-theme 'tron-legacy t)
  :custom (tron-legacy-theme-vivid-cursor t))


;; SOLARIZED
(use-package solarized-theme
  :disabled t
  :config (load-theme 'solarized-light t)
  :custom (solarized-high-contrast-mode-line t))

;; - LIGHT -
;; FLATUI
;; SOLARIZED

;;; ;;; ;;; ;;; ;;; ;;; ;;; END PACKAGES  ;;; ;;; ;;; ;;; ;;; ;;; 

;; This allows customizations to be written to the "~/.emacs" file
(provide '.emacs)
;;; .emacs ends here
