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

;; Helper function to map major-mode to language string
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

;; Main command to edit/generate current line
(defun gptel-edit-line ()
  "Edit or generate the current line using AI.
If the line is empty, generates new code.
If the line has content, edits/improves it.
Uses language context from major-mode."
  (interactive)
  (when (buffer-modified-p)
    (when (y-or-n-p "Buffer has unsaved changes. Save before editing? ")
      (save-buffer)))

  ;; Check if buffer is read-only
  (when buffer-read-only
    (user-error "Buffer is read-only"))

  (let* ((line-start (line-beginning-position))
         (line-end (line-end-position))
         (line-content (buffer-substring-no-properties line-start line-end))
         (line-is-empty (string-match-p "^[[:space:]]*$" line-content))
         (current-language (gptel--mode-to-language))
         (indentation (save-excursion
                        (goto-char line-start)
                        (skip-chars-forward " \t")
                        (buffer-substring-no-properties line-start (point))))
         (system-prompt "You are a code assistant. Return EXACTLY ONE line of code. No explanations, no markdown formatting, no backticks, no line numbers. Just the single line of code.")
         (user-prompt (if line-is-empty
                          (format "Generate one line of %s code." current-language)
                        (format "Edit this %s code to improve it (return exactly one line):\n%s"
                                current-language
                                (string-trim line-content)))))

    ;; Make the API request
    (message "Requesting AI edit...")
    (gptel-request user-prompt
      :system system-prompt
      :temperature 0.2
      :max-tokens 100
      :callback
      (lambda (response info)
        (if-not response
            (message "Error: %s" (plist-get info :status))
          ;; Process and insert the response
          (let* ((clean-response (string-trim response))
                 ;; Remove markdown code fences if present
                 (clean-response (replace-regexp-in-string "^```.*\n?" "" clean-response))
                 (clean-response (replace-regexp-in-string "\n?```$" "" clean-response))
                 ;; Remove any remaining newlines
                 (clean-response (replace-regexp-in-string "\n" " " clean-response))
                 (final-line (concat indentation clean-response)))

            (if (string-empty-p clean-response)
                (message "Warning: Empty response from API")
              ;; Replace the line atomically
              (save-excursion
                (goto-char line-start)
                (delete-region line-start line-end)
                (insert final-line))
              (message "Line updated successfully"))))))))

(provide 'gptel)
