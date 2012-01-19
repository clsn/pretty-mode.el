;;; pretty-mode.el --- redisplay parts of the buffer as pretty symbols
;;; -*- coding: utf-8 -*-

;;; Commentary:
;; 
;; Minor mode for redisplaying parts of the buffer as pretty symbols
;; originally modified from Trent Buck's version at http://paste.lisp.org/display/42335,2/raw
;; Also includes code from `sml-mode'
;; See also http://www.emacswiki.org/cgi-bin/wiki/PrettyLambda
;; 
;; Released under the GPL. No implied warranties, etc. Use at your own risk.
;; Arthur Danskin <arthurdanskin@gmail.com>, March 2008
;;
;; to install:
;; (require 'pretty-mode)
;; and 
;; (global-pretty-mode 1)
;; or
;; (add-hook 'my-pretty-language-hook 'turn-on-pretty-mode)

;; Mark Shoulson: playing around with symbols that look fun.

;;; Code:
(require 'cl)

(defvar pretty-syntax-types '(?_ ?w))

;; modified from `sml-mode'
(defun pretty-font-lock-compose-symbol (alist)
  "Compose a sequence of ascii chars into a symbol."
  (let* ((start (match-beginning 0))
	 (end (match-end 0))
         (syntax (char-syntax (char-after start)))
	 (beforestart (1- start))
	 (afterend (1+ end)))
    ; Can I find a way to let this allow ' ' and " " through?
    ; maybe... start and end are quote-face but beforestart and beforeend
    ; are not?  Then we would have the whole quote.
    ; The statement's conditions are not well-suited to this
    ; kind of exception...
    ; Tried, doesn't work. :(
    ;; can't even do it with straightforward exceptions...
;;;    (display-message-or-buffer 
;;;     (pp-to-string (list (and start 
;;;			      (get-text-property start 'face)))))
    (if 
	(and
	 (or 
	  (if (memq syntax pretty-syntax-types)
	      (or (memq (char-syntax (char-before start)) pretty-syntax-types)
		  (memq (char-syntax (char-after end)) pretty-syntax-types))
	    (memq (char-syntax (char-before start)) '(?. ?\\)))
	  (memq (get-text-property start 'face)
		'(font-lock-doc-face font-lock-string-face
				     font-lock-comment-face)))
	 (not (and (and start		; FAILS INSIDE STRINGS ?
			(equal (char-syntax (char-after start)) ?\"))
		   (and end
			(equal (char-syntax (char-before end)) ?\"))))
	 )
	(remove-text-properties start end '(composition))
      (compose-region start end (cdr (assoc (match-string 0) alist)))
;;;       (add-text-properties start end `(display ,repl)))
      ))
  ;; Return nil because we're not adding any face property.
  nil)

(defvar pretty-interaction-mode-alist
  '((inferior-scheme-mode . scheme-mode)
    (lisp-interaction-mode . emacs-lisp-mode)
    (inferior-lisp-mode . lisp-mode)
    (inferior-ess-mode . ess-mode)
    (inf-haskell-mode . haskell-mode)
    (tuareg-interactive-mode . tuareg-mode)
    (inferior-python-mode . python-mode)
    (inferior-octave-mode . octave-mode)
    (inferior-ruby-mode . ruby-mode))
  "Alist mapping from inferior process interaction modes to their
  corresponding script editing modes.")


(defun pretty-font-lock-keywords (alist)
  "Return a `font-lock-keywords' style entry for replacing
regular expressions with symbols. ALIST has the form ((STRING .
REPLACE-CHAR) ...)."
  (when alist
    `((,(regexp-opt (mapcar 'car alist))
       (0 (pretty-font-lock-compose-symbol
           ',alist))))))

(defun pretty-keywords (&optional mode)
  "Return the font-lock keywords for MODE, or the current mode if
MODE is nil. Return nil if there are no keywords."
  (let* ((mode (or mode major-mode))
         (kwds (cdr-safe
                (or (assoc mode pretty-patterns)
                    (assoc (cdr-safe
                            (assoc mode pretty-interaction-mode-alist))
                           pretty-patterns)))))
    (pretty-font-lock-keywords kwds)))

(defgroup pretty nil "Minor mode for replacing text with symbols "
  :group 'faces)

(define-minor-mode pretty-mode
  "Toggle Pretty minor mode.
With arg, turn Pretty minor mode on if arg is positive, off otherwise.

Pretty mode builds on `font-lock-mode'. Instead of highlighting
keywords, it replaces them with symbols. For example, lambda is
displayed as λ in lisp modes."
  :group 'pretty
;  :lighter " λ"
  (if pretty-mode
      (progn
        (font-lock-add-keywords nil (pretty-keywords) t)
        (font-lock-fontify-buffer))
    (font-lock-remove-keywords nil (pretty-keywords))
    (remove-text-properties (point-min) (point-max) '(composition nil))))

(defun turn-on-pretty-if-desired ()
  "Turn on `pretty-mode' if the current major mode supports it."
  (if (pretty-keywords)
      (pretty-mode 1)))

(define-globalized-minor-mode global-pretty-mode
  pretty-mode turn-on-pretty-if-desired
  :init-value t)

(defun turn-off-pretty-mode ()
  (interactive)
  (pretty-mode -1))


(defun turn-on-pretty-mode ()
  (interactive)
  (pretty-mode +1))

(defun pretty-compile-patterns (patterns)
  "Set pretty patterns in a convenient way.

PATTERNS should be of the form ((GLYPH (REGEXP MODE ...) ...)
...). GLYPH should be a character. MODE should be the name of a
major mode without the \"-mode\". Returns patterns in the form
expected by `pretty-patterns'"
  (let ((pretty-patterns))
    (loop for (glyph . pairs) in patterns do
          (loop for (regexp . major-modes) in pairs do
                (loop for mode in major-modes do
                      (let* ((mode (intern (concat (symbol-name mode)
                                                   "-mode")))
                             (assoc-pair (assoc mode pretty-patterns))
                            
                             (entry (cons regexp glyph)))
                        (if assoc-pair
                            (push entry (cdr assoc-pair))
                          (push (cons mode (list entry))
                                pretty-patterns))))))
    pretty-patterns))

;; I am adding a great many of these which I will freely admit to being
;; there simply because I can and am trying to maximize "stuff" being done.
(defvar pretty-patterns
  (let* ((lispy '(scheme emacs-lisp lisp))
         (mley '(tuareg haskell sml))
         (c-like '(c c++ perl sh python java ess ruby))
         (all (append lispy mley c-like (list 'octave))))
    (pretty-compile-patterns
     `(
       (?≠ ("!=" ,@c-like scheme octave)
           ("<>" tuareg octave)
           ("~=" octave)
           ("/=" haskell emacs-lisp))
       (?≡ ("is" python))
       (?≢ ("is not" python))
       (?≤ ("<=" ,@all))
       (?≥ (">=" ,@all))
       (?← ("<-" ,@mley ess))
       (?➛ ("->" ,@mley ess c c++ perl))
       (?↑ ("\\^" tuareg))
       (?⇒ ("=>" sml perl ruby haskell))
       ; (?⟹ ("=>" sml perl ruby haskell)) ;too long
       (?∅ ("nil" emacs-lisp ruby)
           ("null" scheme java)
           ("NULL" c c++)
	   ("None" python)
           ("()" ,@mley))
       (?␣ ("' '" perl python)		; DOES NOT WORK -- now sorta works
	   ("\" \"" ,@all)		; we don't prettify quoted strings.
	   ("q( )" perl))	  ; (nobody uses this in perl, but it works)
       (?ϵ ("''" perl python)		; DOES NOT WORK.  (Better symbol?)
	   ("\"\"" ,@all)		; maybe we can think of something.
	   ("q()" perl))	   ; (nobody uses this in perl, but it works)
       (?‴ ("\"\"\"" python)	   ; mainly to prevent conflicts with ""
	   ("'''" python))
       (?≟ ("==" ,@all))	   ; so what, having fun.
       (?… ("\\.\\.\\." scheme perl))	; perl6
       (?‥ ("\\.\\." perl))		; maybe hard to read
;;;    (?∀ ("List.for_all" tuareg))
       (?∀ ("all" tuareg perl python))		; perl6
;;;    (?∃ ("List.exists" tuareg))
       (?∃ ("any" perl python))		; perl6
       (?∄ ("none" perl))		; perl6
       (?∈ ("in" python))
;;;    (?∈ ("List.mem" tuareg)
;;;        ("member" ,@lispy))       
       (?∉ ("not in" python))
       (?√ ("sqrt" ,@all))
       (?∑ ("sum" python))
       (?ℤ ("int" python))		; ☺
       (?ℝ ("float" python))
       (?ℂ ("complex" python))
;;;    (?⅀ ("str" python))    ; too obscure
       (?α ("alpha" ,@all)
	   ("'a" python)
           ("'a" ,@mley))
       (?β ("beta" ,@all)
           ("'b" ,@mley))
       (?γ ("gamma" ,@all)
           ("'c" ,@mley))
       (?Δ ("delta" ,@all)
           ("'d" ,@mley))
       (?ε ("epsilon" ,@all))
       (?θ ("theta" ,@all))
       (?λ ("lambda" ,@all)
;;;        ("case-\\(lambda\\)" scheme)
           ("fn" sml)
           ("fun" tuareg)
           ("\\" haskell))
       (?π ("pi" ,@all)
           ("M_PI" c c++))
       (?τ ("tau" ,@all))
       (?φ ("phi" ,@all))
       (?ψ ("psi" ,@all))

       (?² ("**2" python tuareg octave)
           ("^2" octave haskell))
       (?³ ("**3" python tuareg octave)
           ("^3" octave haskell))
       (?ⁿ ("**n" python tuareg octave)
           ("^n" octave haskell))

; I like these, but they'll never work in practice. (will they?)
; Oh, let's try just [0], since that's commonly used.
       (?₀ ("[0]" ,@c-like))
; dumb idea, but I have to at least play with it.
    ;;     ("(0)" octave)
    ;;     (".(0)" tuareg))
    ;; (?₁ ("[1]" ,@c-like)
    ;;     ("(1)" octave)
    ;;     (".(1)" tuareg))
    ;; (?₂ ("[2]" ,@c-like)
    ;;     ("(2)" octave)
    ;;     (".(2)" tuareg))
    ;; (?₃ ("[3]" ,@c-like)
    ;;     ("(3)" octave)
    ;;     (".(3)" tuareg))
    ;; (?₄ ("[4]" ,@c-like)
    ;;     ("(4)" octave)
    ;;     (".(4)" tuareg))

       (?∞ ("HUGE_VAL" c c++))

;;;    (?∙ ())
;;;    (?× ())
;;;    (?ₐ ("[a]" ,@c-like)
;;;        ("(a)" octave))
;;;    (?ₓ ("[x]" ,@c-like)
;;;        ("(x)" octave))
;;;    (?₅ ("[5]") ,@c-like)
;;;    (?₆ ("[6]") ,@c-like)
;;;    (?₇ ("[7]") ,@c-like)
;;;    (?₈ ("[8]") ,@c-like)
;;;    (?₉ ("[9]") ,@c-like)

;;;    (?⋂ "\\<intersection\\>"   (,@lispen))
;;;    (?⋃ "\\<union\\>"          (,@lispen))

   
;;;    (?∧ ("\\<And\\>"     emacs-lisp lisp python)
;;;        ("\\<andalso\\>" sml)
       (?⋀ ("and" python perl)) ;careful not to conflate or and || in perl.
;;;	   ("&&"            c c++ perl haskell))
;;;    (?∨ ("\\<or\\>"      emacs-lisp lisp)
       (?⋁ ("or" python perl))  ; N-ARY LOGICAL OR looks less like v
;;;        ("\\<orelse\\>"  sml)
;;;	   ("||"            c c++ perl haskell))
       (?¬ ("!"       c c++ perl sh)
;;;        ("\\<not\\>"     lisp emacs-lisp scheme haskell sml))
	   ("not" python perl))
       ;; These?  Probably dumb.
       (?■ ("True" python perl))
       (?□ ("False" python perl))

       )))
    "*List of pretty patterns.

Should be a list of the form ((MODE ((REGEXP . GLYPH) ...)) ...)")


(defun pretty-add-keywords (mode keywords)
  "Add pretty character KEYWORDS to MODE

MODE should be a symbol, the major mode command name, such as
`c-mode' or nil. If nil, pretty keywords are added to the current
buffer. KEYWORDS should be a list where each element has the
form (REGEXP . CHAR). REGEXP will be replaced with CHAR in the
relevant buffer(s)."
  (font-lock-add-keywords
   mode (mapcar (lambda (kw) `(,(car kw)
                          (0 (prog1 nil
                               (compose-region (match-beginning 0)
                                               (match-end 0)
                                               ,(cdr kw))))))
                keywords)))

(defun pretty-regexp (regexp glyph)
  "Replace REGEXP with GLYPH in buffer."
  (interactive "MRegexp to replace: 
MCharacter to replace with: ")
  (pretty-add-keywords nil `((,regexp . ,(string-to-char glyph))))
  (font-lock-fontify-buffer))




(provide 'pretty-mode)
