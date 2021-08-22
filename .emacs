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

(defun add-prettify-symbols ()
  "make some word or string show as pretty Unicode symbols"
  (setq prettify-symbols-alist
        '(;; Greek
          ("lambda" . 955) ; λ
	  ("pi" . 960)

	  ;; Assignment
	  ("let" . 8592)
	  ("let*" . 8594)
	  ("flet" . 8797)
					;	  ("defun" . 8614)   ; ↦
	  ("defun" . 10236)
	  ("setf" . 8788)
	  
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
	  ("and" . 8896)
	  ("or" . 8897)
	  ("cond" . 8866)

	  ;; Math Operators
	  ("/" . 247)
	  ("*" . 215)
	  ("sqrt" . 8730)

	  ;; Sequences
	  ("'()" . 8709)
	  ("member" . 8715)
	  ("member-if" . 8957)
	  ("member-if-not" . 8716)
	  ("elt" . 8712)
	  ("subset" . 8834)
	  ("union" . 8899)
	  ("intersection" . 8898)
	  ("append" . 10756)
	  ("length" . 8594)

	  ;; Maps

	  ("map" . 10204)
	  ("mapcar" . 8887)
	  ("maplist" . 8886)
	  ("mapc" . 8888)

	  ;; Order
	  ("<" . 8826)
	  (">" . 8827)
	  ("<=" . 8828)
	  (">=" . 8829)
	  
	  ;; Arrows
          ("->" . 8594)    ; →
          ("=>" . 8658)    ; ⇒

          )))

(add-hook 'lisp-mode-hook 'add-prettify-symbols)
(add-hook 'sly-mode-hook 'add-prettify-symbols)

