;; M-x guix-emacs-autoload-packages

(setq inhibit-startup-screen t)                ; Initial window and default window
(setq initial-scratch-message ";; Hi Jason!")  ; Don't use messages that you don't read
(tool-bar-mode -1)
(column-number-mode 1)
(blink-cursor-mode 0)

;; set highlighting brackets
(progn (show-paren-mode 1)
       (setq show-paren-style 'parenthesis))

;; set default font
(when (member "IBM Plex Mono" (font-family-list))
  (set-frame-font "IBM Plex Mono-13" t t))

(set-default 'truncate-lines nil)  ; Turn off Line Wrap
(setq visible-bell t)              ; Turn off Audio Bell
(display-time)
(fset 'yes-or-no-p 'y-or-no-p)     ; Always answer 'y' or 'n'
(setq backup-directory-alist `(("." . "~/.emacs.saves")))

;;; Display full path to the file in minibuffer
(defun show-file-name ()
  "Show the full path file name in the minibuffer"
  (interactive)
  (message (buffer-file-name)))

(global-set-key (kbd "<f5>") 'show-file-name)

;; Easily move point between windows using Shift+<Arrow Key>
(windmove-default-keybindings)

;; Encoding
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(global-prettify-symbols-mode 1)  ; display “lambda” as “λ”
(which-key-mode)
(global-company-mode)

;;;     LISP --- Everything related to Common Lisp Development       ;;;

(add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
(add-hook 'lisp-mode-hook (lambda () (lispy-mode 1)))

(global-paren-face-mode)

;;;            HTML/CSS/JS --- Web Development packages              ;;;

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.clp\\'" . web-mode))
(web-mode-enable-auto-closing t))

;;;           COLOUR THEMES --- Look and feel of the editor          ;;;

;; M-x load-theme 
;; OR C-x C-e at the end of the theme settings

(setq solarized-high-contrast-mode-line t)
(load-theme 'solarized-light t)

;; This allows customizations to be written to the "~/.emacs" file
(provide '.emacs)
