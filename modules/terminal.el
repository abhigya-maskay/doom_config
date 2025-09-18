;;; modules/terminal.el -*- lexical-binding: t; -*-

;; Eat terminal configuration
(setq eat-term-name "xterm-256color")

(use-package! eat
  :commands (eat eat-other-window))

(after! eat
  (setq eat-kill-buffer-on-exit nil)
  (add-hook 'eat-exec-hook
            (lambda (_process)
              (add-hook 'eat-exit-hook
                        (lambda (proc)
                          (when-let ((buffer (process-buffer proc)))
                            (dolist (win (get-buffer-window-list buffer nil t))
                              (when (window-live-p win)
                                (delete-window win)))))
                        nil t))))

;; Helper functions for eat terminal
(defun +eat--workspace-name ()
  (if (bound-and-true-p persp-mode)
      (safe-persp-name (get-current-persp))
    "main"))

(defun +eat--popup-buffer-name ()
  (format "*doom:eat-popup:%s*" (+eat--workspace-name)))

(defun +eat--here-buffer-name (project-root)
  (if project-root
      (format "*doom:eat:%s*"
              (file-name-nondirectory (directory-file-name project-root)))
    "*doom:eat*"))

(defun +eat--project-root ()
  (or (doom-project-root) default-directory))

(defun +eat--ensure ()
  (or (require 'eat nil 'noerror)
      (user-error "EAT is not available yet; run `doom sync` and restart Emacs")))

(defun +eat--with-root (arg fn)
  (+eat--ensure)
  (let* ((origin default-directory)
         (project-root (+eat--project-root))
         (target (if arg origin project-root)))
    (let ((default-directory target))
      (setenv "PROOT" project-root)
      (funcall fn target project-root arg))))

(defun +eat/toggle (arg)
  (interactive "P")
  (+eat--with-root
   arg
   (lambda (_dir _project-root inner-arg)
     (let* ((buffer-name (+eat--popup-buffer-name))
            (buffer (get-buffer buffer-name)))
       (when inner-arg
         (when-let (win (get-buffer-window buffer))
           (delete-window win))
         (when buffer
           (kill-buffer buffer)
           (setq buffer nil)))
       (if-let ((win (get-buffer-window buffer-name)))
           (progn
             (delete-window win)
             (or buffer (get-buffer buffer-name)))
         (let ((eat-buffer-name buffer-name))
           (eat-other-window nil inner-arg))
         (get-buffer buffer-name))))))

(defun +eat/here (arg)
  (interactive "P")
  (+eat--with-root
   arg
   (lambda (_dir project-root inner-arg)
     (let ((eat-buffer-name (+eat--here-buffer-name project-root)))
       (eat nil inner-arg)))))
