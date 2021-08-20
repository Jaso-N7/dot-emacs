(load "~/.emacs.d/init.el")

;; Initial window and default window
(setq inhibit-startup-screen t)

;; User interface
(menu-bar-mode -1)
(tool-bar-mode -1)
(column-number-mode 1)
(blink-cursor-mode 0)

;; set highlighting brackets
(show-paren-mode 1)
(setq show-paren-style 'parenthesis)

;; display “lambda” as “λ”
(global-prettify-symbols-mode 1)

