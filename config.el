;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Abhigya Maskay")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'catppuccin)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; evil escape sequence
(setq evil-escape-key-sequence "fd")

;; Show battery and time
(after! doom-modeline
  (display-time-mode 1)
  (display-battery-mode 1))

;; Change Font
(setq doom-font (font-spec :size 18 :family "Operator Mono Lig"))

(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-comment-delimiter-face :slant italic)
  '(font-lock-type-face :slant italic)
  '(font-lock-doc-face :slant italic)
  '(font-lock-constant-face :slant italic)
  '(font-lock-builtin-face :slant italic))

;; disable angular lsp
(after! lsp-mode
  (add-to-list 'lsp-disabled-clients 'angular-ls))

;; python formatter
;;(after! python
;;  (set-formatter! 'black "black -q -" :modes '(python-mode)))

;; haskell formatter
(after! lsp-haskell
  (setq lsp-haskell-formatting-provider "ormolu"))

;; relative line numbering
(setq display-line-numbers-type 'relative)

;; org mode lsp enable
(defun org-babel-edit-prep:haskell (babel-info)
  (setq-local buffer-file-name (->> babel-info caddr (alist-get :tangle)))
  (lsp))

;; Gemini api key for editor integration;;
(setq
 gptel-model 'gemini-2.5-flash-preview-05-20
 gptel-backend (gptel-make-gemini "Gemini"
                 :key (auth-source-pick-first-password :host "generativelanguage.googleapis.com")
                 :stream t))

(map!
 :leader
 :desc "Calls the gptel function with configured backend"
 "c g" #'gptel)

(map!
 :leader
 :desc "Calls gptel menu"
 "c m" #'gptel-menu)

(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package! org-roam
  :custom
  (org-roam-directory "~/org-roam"))

(setq gptel-log-level 'info)

;; Cabal file handling
(add-to-list 'auto-mode-alist '("\\.cabal\\'" . haskell-cabal-mode))
(add-hook 'haskell-cabal-mode-hook #'lsp)

;; Add prettified symbols so they work with all fonts
(global-prettify-symbols-mode 1)

(defvar my/prettify-symbols-alist
  '(;; Arrows and flow
    ("->" . "→")
    ("<-" . "←")
    ("=>" . "⇒")
    ("<=" . "⇐")
    ("<=>" . "⇔")
    ("=<<" . "≪")
    (">>=" . "≫")
    (">->" . "↣")
    ("<-<" . "↢")
    ("->>" . "↠")
    ("<<-" . "↞")

    ;; Equality and comparison
    ("==" . "≡")
    ("!=" . "≠")
    ("/=" . "≠")
    (">=" . "≥")
    ("<=" . "≤")
    ("===" . "≣")
    ("!==" . "≢")

    ;; Mathematical operators
    ("++" . "⧺")
    ("--" . "⸺")
    ("**" . "×")
    ("*/" . "⋆")
    ("//" . "÷")
    ("%" . "℅")
    ("^" . "↑")
    ("^^" . "⤴")

    ;; Logical operators
    ("&&" . "∧")
    ("||" . "∨")
    ("!" . "¬")
    ("not" . "¬")
    ("and" . "∧")
    ("or" . "∨")

    ;; Functional programming
    ("<$>" . "⊛")
    ("<*>" . "⊛")
    ("<|>" . "⊕")
    ("<>" . "◇")
    ("mappend" . "⊕")
    ("mempty" . "∅")
    ("." . "·")
    ("compose" . "∘")
    ("lambda" . "λ")
    ("\\" . "λ")

    ;; Type annotations
    ("::" . "∷")
    (":" . "∶")

    ;; Ruby-specific
    ("<=>" . "⇔")
    ("=~" . "≈")
    ("!~" . "≉")
    ("<<" . "≪")
    (">>" . "≫")

    ;; JavaScript/Python
    ("===" . "≣")
    ("!==" . "≢")
    ("..." . "…")

    ;; Brackets and delimiters
    ("[]" . "⊡")
    ("{}" . "⊙")
    ("()" . "○")

    ;; Special symbols
    ("null" . "∅")
    ("nil" . "∅")
    ("None" . "∅")
    ("undefined" . "⊥")
    ("false" . "⊥")
    ("true" . "⊤")
    ("True" . "⊤")
    ("False" . "⊥")

    ;; Infinity and limits
    ("infinity" . "∞")
    ("inf" . "∞")

    ;; Set operations
    ("union" . "∪")
    ("intersection" . "∩")
    ("in" . "∈")
    ("not in" . "∉")
    ("subset" . "⊂")
    ("superset" . "⊃")

    ;; Greek letters commonly used in programming
    ("alpha" . "α")
    ("beta" . "β")
    ("gamma" . "γ")
    ("delta" . "δ")
    ("epsilon" . "ε")
    ("theta" . "θ")
    ("lambda" . "λ")
    ("mu" . "μ")
    ("pi" . "π")
    ("sigma" . "σ")
    ("tau" . "τ")
    ("phi" . "φ")
    ("chi" . "χ")
    ("psi" . "ψ")
    ("omega" . "ω")

    ;; Quantifiers
    ("forall" . "∀")
    ("exists" . "∃")
    ("nexists" . "∄")

    ;; Additional arrows
    ("|->" . "↦")
    ("<-|" . "↤")
    ("=>" . "⇒")
    ("<=" . "⇐")
    ("<=>" . "⇔")
    ("~/>" . "↝")
    ("<~" . "↜")))

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

;; copilot setup
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)
              ("C-n" . 'copilot-next-completion)
              ("C-p" . 'copilot-previous-completion)))

;; Add org mode keybinds
(map!
   :after org
   :mode org-mode
   :localleader
   "t" #'org-babel-tangle)

(map!
   :leader
   "c T" #'org-babel-detangle)
