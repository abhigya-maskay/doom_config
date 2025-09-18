;;; modules/languages.el -*- lexical-binding: t; -*-

;; Angular LSP - disable it
(after! lsp-mode
  (add-to-list 'lsp-disabled-clients 'angular-ls))

;; Python configuration
;; Python formatter (commented out)
;;(after! python
;;  (set-formatter! 'black "black -q -" :modes '(python-mode)))

;; Python LSP server
(setq lsp-pyright-langserver-command "basedpyright")

;; Haskell configuration
(after! lsp-haskell
  (setq lsp-haskell-formatting-provider "ormolu")
  (setq lsp-haskell-server-path "haskell-language-server-wrapper"))

;; Org mode babel for Haskell with LSP support
(defun org-babel-edit-prep:haskell (babel-info)
  (setq-local buffer-file-name (->> babel-info caddr (alist-get :tangle)))
  (lsp))

;; Cabal file handling
(add-to-list 'auto-mode-alist '("\\.cabal\\'" . haskell-cabal-mode))
(add-hook 'haskell-cabal-mode-hook #'lsp)