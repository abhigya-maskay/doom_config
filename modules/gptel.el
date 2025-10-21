;;; modules/gptel.el -*- lexical-binding: t; -*-

;; Configure gptel backend for local API
(after! gptel
  ;; Define custom OpenAI-compatible backend
  (gptel-make-openai "Local API"
    :host "localhost:8317"
    :protocol "http"
    :endpoint "/v1/chat/completions"
    :stream nil  ; Disable streaming for atomic replacements
    :key (lambda ()
           ;; Retrieve API key from auth-source
           (if-let ((secret (plist-get
                             (car (auth-source-search :host "localhost"
                                                       :require '(:secret)))
                             :secret)))
               (if (functionp secret)
                   (funcall secret)
                 secret)
             (error "No API key found in auth-source for host 'localhost'")))
    :models '("claude-haiku-4-5-20251001"))

  ;; Set default model
  (setq-default gptel-model "claude-haiku-4-5-20251001"
                gptel-backend (gptel-make-openai "Local API"
                                :host "localhost:8317"
                                :protocol "http"
                                :endpoint "/v1/chat/completions"
                                :stream nil
                                :key (lambda ()
                                       (if-let ((secret (plist-get
                                                         (car (auth-source-search :host "localhost"
                                                                                   :require '(:secret)))
                                                         :secret)))
                                           (if (functionp secret)
                                               (funcall secret)
                                             secret)
                                         (error "No API key found in auth-source for host 'localhost'")))
                                :models '("claude-haiku-4-5-20251001"))))

;;;###autoload
(defun gptel--mode-to-language ()
  "Convert current major-mode to a language string for the API."
  (pcase major-mode
    ('rust-mode "rust")
    ('rustic-mode "rust")
    ('python-mode "python")
    ('python-ts-mode "python")
    ('emacs-lisp-mode "elisp")
    ('lisp-interaction-mode "elisp")
    ('js-mode "javascript")
    ('js2-mode "javascript")
    ('javascript-mode "javascript")
    ('typescript-mode "typescript")
    ('typescript-ts-mode "typescript")
    ('ruby-mode "ruby")
    ('go-mode "go")
    ('java-mode "java")
    ('c-mode "c")
    ('c++-mode "c++")
    ('haskell-mode "haskell")
    ('nix-mode "nix")
    ('sh-mode "shell")
    ('bash-ts-mode "bash")
    ('markdown-mode "markdown")
    ('org-mode "org")
    ('yaml-mode "yaml")
    ('json-mode "json")
    ('html-mode "html")
    ('css-mode "css")
    (_ "plain text")))

;;;###autoload
(defun gptel-edit-line ()
  "Edit or generate the current line using AI.
If the line is empty, generates new code.
If the line has content, edits/improves it.
Uses language context from major-mode."
  (interactive)
  (require 'gptel)
  (when buffer-read-only
    (user-error "Buffer is read-only"))

  (let* ((line-start (line-beginning-position))
         (line-end (line-end-position))
         (line-content (buffer-substring-no-properties line-start line-end))
         (line-is-empty (string-match-p "^[[:space:]]*$" line-content))
         (current-language (gptel--mode-to-language))
         (instruction (string-trim
                       (read-string (if line-is-empty
                                        "Describe what to generate (leave blank for auto): "
                                      "Describe the change for this line (leave blank for auto): "))))
         (system-prompt "You are a code assistant. Return EXACTLY ONE line of code. No explanations, no markdown formatting, no backticks, no line numbers. Just the single line of code.")
         (user-prompt (if line-is-empty
                          (if (string-empty-p instruction)
                              (format "Generate one line of %s code." current-language)
                            (format "Generate one line of %s code that follows these instructions:\n%s"
                                    current-language
                                    instruction))
                        (if (string-empty-p instruction)
                            (format "Edit this %s code to improve it (return exactly one line):\n%s"
                                    current-language
                                    (string-trim line-content))
                          (format (concat "You are editing this %s line.\n"
                                          "Current line:\n%s\n"
                                          "Apply these instructions and return exactly one line:\n%s")
                                  current-language
                                  (string-trim line-content)
                                  instruction)))))

    (message "Requesting AI edit...")
    (gptel-request user-prompt
      :system system-prompt
      :callback
      (lambda (response info)
        (if (not response)
            (message "Error: %s" (plist-get info :status))
          (let* ((clean-response (string-trim response))
                 (clean-response (replace-regexp-in-string "^```.*\n?" "" clean-response))
                 (clean-response (replace-regexp-in-string "\n?```$" "" clean-response))
                 (clean-response (replace-regexp-in-string "\n" " " clean-response))
                 (final-line clean-response))

            (if (string-empty-p clean-response)
                (message "Warning: Empty response from API")
              (save-excursion
                (goto-char line-start)
                (delete-region line-start line-end)
                (insert final-line)
                (indent-according-to-mode))
              (message "Line updated successfully"))))))))

(provide 'modules-gptel)
