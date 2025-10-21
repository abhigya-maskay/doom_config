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

;; Inline instruction prompt helpers
(defvar gptel-edit-line--instruction-buffer " *gptel instruction*"
  "Buffer used to collect instructions for `gptel-edit-line'.")

(defvar gptel-edit-line--instruction-result nil
  "Internal storage for the current instruction entry result.")

(defvar gptel-edit-line-instruction-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map (kbd "C-c C-c") #'gptel-edit-line-instruction-confirm)
    (define-key map (kbd "C-c C-k") #'gptel-edit-line-instruction-cancel)
    map)
  "Keymap used while entering GPTel edit instructions.")

(define-derived-mode gptel-edit-line-instruction-mode text-mode "GPTel-Instruction"
  "Major mode for entering instructions for `gptel-edit-line'."
  (setq-local truncate-lines t)
  (setq-local header-line-format "Describe the change. C-c C-c to accept, C-c C-k to cancel."))

(defun gptel-edit-line-instruction-confirm ()
  "Confirm the current instruction entry."
  (interactive)
  (setq gptel-edit-line--instruction-result
        (string-trim (buffer-substring-no-properties (point-min) (point-max))))
  (exit-recursive-edit))

(defun gptel-edit-line-instruction-cancel ()
  "Cancel the current instruction entry."
  (interactive)
  (abort-recursive-edit))

(defun gptel-edit-line--prompt-instruction (prompt)
  "Prompt for an instruction using a temporary inline buffer with PROMPT.
Returns the trimmed instruction string, or nil if the entry is cancelled."
  (let* ((buffer (get-buffer-create gptel-edit-line--instruction-buffer))
         (window (or (display-buffer buffer '(display-buffer-below-selected))
                     (error "Could not display instruction buffer")))
         (gptel-edit-line--instruction-result nil))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (gptel-edit-line-instruction-mode)
            (erase-buffer)
            (setq-local header-line-format prompt)
            (goto-char (point-min)))
          (with-selected-window window
            (goto-char (point-max))
            (fit-window-to-buffer window 10))
          (condition-case nil
              (progn
                (select-window window)
                (recursive-edit)
                gptel-edit-line--instruction-result)
            (quit nil)))
      (when (window-live-p window)
        (quit-window 'kill window)))))

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
         (prompt-text (if line-is-empty
                          "Describe what to generate (C-c C-c to accept, C-c C-k to cancel): "
                        "Describe the change for this line (C-c C-c to accept, C-c C-k to cancel): "))
         (instruction (gptel-edit-line--prompt-instruction prompt-text)))
    (if (null instruction)
        (message "Instruction entry cancelled")
      (let* ((system-prompt "You are a code assistant. Perform only the requested change and nothing else. Return EXACTLY ONE line of code. No explanations, no markdown formatting, no backticks, no line numbers. Just the single line of code.")
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
                  (message "Line updated successfully"))))))))))

(provide 'modules-gptel)
