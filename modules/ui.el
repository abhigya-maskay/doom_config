;;; modules/ui.el -*- lexical-binding: t; -*-

;; Theme configuration
(setq doom-theme 'catppuccin)

;; Line numbers configuration
(setq display-line-numbers-type 'relative)

;; Font configuration
(setq doom-font (font-spec :size 18 :family "Monaspace Krypton"))

(custom-set-faces!
  '(font-lock-comment-face :slant italic :family "Monaspace Radon")
  '(font-lock-comment-delimiter-face :slant italic :family "Monaspace Radon")
  '(font-lock-type-face :slant italic :family "Monaspace Radon")
  '(font-lock-doc-face :slant italic :family "Monaspace Radon")
  '(font-lock-constant-face :slant italic :family "Monaspace Radon")
  '(font-lock-builtin-face :slant italic :family "Monaspace Radon"))

;; Modeline configuration
(after! doom-modeline
  (display-time-mode 1)
  (display-battery-mode 1))

;; Prettify symbols configuration
(global-prettify-symbols-mode 1)

(defvar my/prettify-symbols-alist
  '(;; Arrows (Haskell, JS, Ruby)
    ("->" . "→")
    ("<-" . "←")
    ("=>" . "⇒")
    (">>=" . "»=")
    ("=<<" . "=«")
    (">>" . "»")
    ("<<" . "«")

    ;; Haskell Functors & Applicatives
    ("<$>" . "⊕")
    ("<*>" . "⊛")
    ("<|>" . "⊘")
    ("<>" . "◇")

    ;; Equality & Comparison (All languages)
    ("==" . "≡")
    ("!=" . "≠")
    ("/=" . "≠")     ; Haskell not equal
    (">=" . "≥")
    ("<=" . "≤")
    ("===" . "≣")    ; JS strict equality
    ("!==" . "≢")    ; JS strict inequality

    ;; Logic (All languages)
    ("&&" . "∧")
    ("||" . "∨")
    ("not" . "¬")    ; Python/Ruby

    ;; List/Array Operations
    ("++" . "⧺")     ; Haskell concatenation
    ("..." . "…")    ; JS spread operator

    ;; Type Annotations (Haskell)
    ("::" . "∷")
    ("forall" . "∀")

    ;; Lambda (Haskell, Python)
    ("\\" . "λ")     ; Haskell
    ("lambda" . "λ") ; Python

    ;; Ruby Operators
    ("<=>" . "⇔")    ; Spaceship operator
    ("=~" . "≈")     ; Pattern match
    ("!~" . "≉")     ; Pattern no match

    ;; Null/Undefined/Empty
    ("null" . "∅")       ; JS
    ("nil" . "∅")        ; Ruby
    ("None" . "∅")       ; Python
    ("undefined" . "⊥")  ; JS/Haskell
    ("mempty" . "∅")     ; Haskell

    ;; Boolean Values
    ("True" . "⊤")   ; Python
    ("False" . "⊥")  ; Python
    ("true" . "⊤")   ; JS/Ruby
    ("false" . "⊥")  ; JS/Ruby

    ;; Math Operations
    ("**" . "^")     ; Python/Ruby exponentiation
    ("//" . "÷")     ; Python floor division
    ("sum" . "∑")    ; Common in all
    ("product" . "∏"); Common in all
    ("sqrt" . "√")   ; Common in all

    ;; Set Operations (Python/Haskell)
    ("union" . "∪")
    ("intersection" . "∩")
    ("in" . "∈")         ; Python membership
    ("not in" . "∉")     ; Python
    ("elem" . "∈")       ; Haskell
    ("notElem" . "∉")    ; Haskell

    ;; Composition (Haskell)
    ("." . "∘")

    ;; Common Mathematical Constants
    ("infinity" . "∞")
    ("inf" . "∞")
    ("pi" . "π")

    ;; Python Specific
    ("and" . "∧")
    ("or" . "∨")

    ;; JS Arrow Functions (keep simple)
    ("() =>" . "λ")

    ;; Greek Letters (commonly used in scientific computing)
    ("alpha" . "α")
    ("beta" . "β")
    ("gamma" . "γ")
    ("delta" . "δ")
    ("epsilon" . "ε")
    ("theta" . "θ")
    ("mu" . "μ")
    ("sigma" . "σ")
    ("tau" . "τ")
    ("phi" . "φ")
    ("omega" . "ω")))

(defun my/setup-prettify-symbols ()
  (setq prettify-symbols-alist my/prettify-symbols-alist))

(add-hook 'haskell-mode-hook #'my/setup-prettify-symbols)
(add-hook 'python-mode-hook #'my/setup-prettify-symbols)
(add-hook 'js-mode-hook #'my/setup-prettify-symbols)
(add-hook 'js2-mode-hook #'my/setup-prettify-symbols)
(add-hook 'ruby-mode-hook #'my/setup-prettify-symbols)
(add-hook 'typescript-mode-hook #'my/setup-prettify-symbols)
(add-hook 'emacs-lisp-mode-hook #'my/setup-prettify-symbols)

(setq prettify-symbols-unprettify-at-point 'right-edge)