;;; modules/org.el -*- lexical-binding: t; -*-

;; Org directory configuration
(setq org-directory "~/org/")

;; Org-roam configuration
(use-package! websocket
  :after org-roam)

(use-package! org-roam
  :custom
  (org-roam-directory "~/org-roam"))

(use-package! org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))