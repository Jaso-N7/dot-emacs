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

;; convenient
(defalias 'yes-or-no-p 'y-or-n-p)

;; set default font
(when (member "Anka/Coder" (font-family-list))
  (set-frame-font "Anka/Coder-13" t t))

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
(add-hook 'sly-mode-hook 'add-prettify-symbols)

