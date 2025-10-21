;;; modules/keybindings.el -*- lexical-binding: t; -*-

;; Evil escape sequence
(setq evil-escape-key-sequence "fd")

;; Eat terminal keybindings
(map!
 :leader
 (:prefix ("o" . "open")
  :desc "Toggle Eat popup" "t" #'+eat/toggle
  :desc "Open Eat here" "T" #'+eat/here))

;; Copilot keybindings
(map!
 :leader
 (:prefix-map ("c g" . "copilot")
              :desc "Copilot related commands"
              ("o" #'copilot-chat-display
               :desc "Open copilot chat")
              ("a" #'copilot-chat-add-current-buffer
               :desc "Add current buffer to copilot chat")
              ("d" #'copilot-chat-del-current-buffer
               :desc "Delete current buffer from copilot chat")
              ("w" #'copilot-chat-add-workspace
               :desc "Add current workspace to copilot chat")
              ("l" #'copilot-chat-list
               :desc "List copilot chat buffers")
              ("h" #'copilot-chat-hide
               :desc "Hide copilot chat buffer")
              ("r" #'copilot-chat-reset
               :desc "Reset entire copilot instance")
              ("s" #'copilot-chat-switch-to-buffer
               :desc "Switch to copilot chat buffer")
              ("m" #'copilot-chat-set-model
               :desc "Switch model for current instance")
              ("g" #'copilot-chat-insert-commit-message
               :desc "Insert commit message into current buffer")
              (:prefix-map ("v" . "copilot work with selected code")
                           ("h" #'copilot-chat-explain
                            :desc "Explain selected code with copilot")
                           ("r" #'copilot-chat-review
                            :desc "Review selected code with copilot")
                           ("d" #'copilot-chat-doc
                            :desc "Document selected code with copilot")
                           ("f" #'copilot-chat-fix
                            :desc "Fix selected code with copilot")
                           ("o" #'copilot-chat-optimize
                            :desc "Optimize selected code with copilot")
                           ("t" #'copilot-chat-test
                            :desc "Test selected code with copilot")
                           ("b" #'copilot-chat-review-buffer
                            :desc "Review entire buffer with copilot"))))

;; gptel keybindings
(map!
 :leader
 (:prefix ("c" . "code")
  :desc "AI edit/generate current line" "i" #'gptel-edit-line))

;; Org mode keybindings
(map!
   :after org
   :mode org-mode
   :localleader
   "t" #'org-babel-tangle)

(map!
   :leader
   "c T" #'org-babel-detangle)