;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; 
;;;           GENERAL SETTINGS --- Overall Emacs settings           ;;;
;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;;

;; Initial window and default window
(setq inhibit-startup-screen t)
;; Don't use messages that you don't read
(setq initial-scratch-message ";; Hi Jason!")

;; Graphical User interface settings
;;(if (fboundp 'menu-bar-mode) (menu-bar-mode 0))
(tool-bar-mode -1)
(column-number-mode 1)
(blink-cursor-mode 0)

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

;;; ;;; ;;; ;;; ;;; ;;; END GENERAL SETTINGS ;;; ;;; ;;; ;;; ;;; ;;; 

;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; 
;;                            PACKAGES                               ;;
;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;;

;; M-x guix-emacs-autoload-packages

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;     LISP --- Everything related to Common Lisp Development       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
(add-hook 'lisp-mode-hook (lambda () (lispy-mode 1)))

(global-paren-face-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;            HTML/CSS/JS --- Web Development packages              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.clp\\'" . web-mode))
(web-mode-enable-auto-closing t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;        MISC --- Convenience and other personal preferences       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(which-key-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;           COLOUR THEMES --- Look and feel of the editor          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; M-x load-theme 
;; OR C-x C-e at the end of the theme settings

;; SOLARIZED
(setq solarized-high-contrast-mode-line t)
(load-theme 'solarized-light t)

;;; ;;; ;;; ;;; ;;; ;;; ;;; END PACKAGES  ;;; ;;; ;;; ;;; ;;; ;;; 

;; This allows customizations to be written to the "~/.emacs" file
(provide '.emacs)
;;; .emacs ends here
