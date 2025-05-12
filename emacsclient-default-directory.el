;;; emacsclient-default-directory.el --- Set default directory for emacsclient buffers -*- lexical-binding: t; -*-

;; Author: Hauke Rehfeld <emacs@haukerehfeld.de>
;; URL: https://github.com/hrehfeld/emacs-emacsclient-default-directory
;; Version: 0.1
;; Package-Requires: ((emacs "24.4"))
;; Keywords: convenience

;;; Commentary:

;; This package provides functions to manage default directories for
;; emacsclient buffers using frame parameters.

;;; Code:

(defvar emacsclient-default-directory-log nil
  "If a string, log messages to the given buffer.

Otherwise if non-nil, log messages to *Messages* buffer.")

(defun emacsclient-default-directory-log (message &rest args)
  "Log a MESSAGE formatted with ARGS according to `emacsclient-default-directory-log'."
  (let ((message (concat "emacsclient-default-directory: " message)))
    (cond ((stringp emacsclient-default-directory-log)
           (let ((log-buffer (get-buffer-create emacsclient-default-directory-log)))
             (with-current-buffer log-buffer
               (save-excursion
                 (goto-char (point-max))
                 (insert (apply #'format message args) "\n")))))
          (emacsclient-default-directory-log
           (apply #'message message args)))))

(defvar emacsclient-default-directory-reset-timeout 5
  "Time in seconds after which the frame parameter `emacsclient-default-directory' is reset.")

(defvar emacsclient-default-directory-reset-timer nil "Timer that will unset the frame parameter that holds last `default-directory'.")

(defcustom emacsclient-default-directory-reset-hooks '(easysession-before-load-hook)
  "List of hooks that should reset the frame parameter `emacsclient-default-directory'.

These are usually hooks that are run before loading files unrelated to the last emacsclient command."
  :type '(repeat symbol)
  :group 'emacsclient-default-directory)

(defcustom emacsclient-default-directory-reset-functions '(activities-resume activities-switch)
  "List of functions that first should reset the frame parameter `emacsclient-default-directory'.

Use this only when no appropriate hook is present.")

(defun emacsclient-default-directory-reset-frame-parameter (&rest args)
  (let ((frame (selected-frame)))
    (when (frame-live-p frame)
      (emacsclient-default-directory-log "Resetting frame parameter (%S)" (frame-parameter frame 'emacsclient-default-directory))
      (set-frame-parameter frame 'emacsclient-default-directory nil)))
  (when emacsclient-default-directory-reset-timer
    (cancel-timer emacsclient-default-directory-reset-timer)
    (setq emacsclient-default-directory-reset-timer nil))
  (remove-hook 'pre-command-hook #'emacsclient-default-directory-reset-frame-parameter))

(defun emacsclient-default-directory-find-file-hook ()
  "Check if the frame parameter `emacsclient-default-directory' is set.
If so, set `default-directory' buffer-locally and reset the frame parameter."
  (let* ((frame (selected-frame))
         (dir (frame-parameter frame 'emacsclient-default-directory)))
    (emacsclient-default-directory-log "find-file-hook: %s %s" dir default-directory)
    (when dir
      (setq-local default-directory dir))
    (setq emacsclient-default-directory-reset-timer
          (run-at-time emacsclient-default-directory-reset-timeout nil #'emacsclient-default-directory-reset-frame-parameter))
    ))

;;;###autoload
(define-minor-mode emacsclient-default-directory-mode
  "Add `emacsclient-default-directory-find-file-hook' to `find-file-hook'."
  :global t
  :lighter nil
  (emacsclient-default-directory-log "init %s. Server: %S" (if emacsclient-default-directory-mode "on" "off") (featurep 'server))
  (with-eval-after-load 'server
    (emacsclient-default-directory-log "server loaded, checking if server is running")
    (if (not server-process)
        (progn
          (add-hook 'server-mode-hook #'emacsclient-default-directory-mode)
          (setq emacsclient-default-directory-mode nil)
          (emacsclient-default-directory-log "server not running, adding to server-start-hook"))
      (let ((hook-action (if emacsclient-default-directory-mode #'add-hook #'remove-hook)))

        ;; add find-file-hook
        (funcall hook-action 'find-file-hook #'emacsclient-default-directory-find-file-hook)

        (funcall hook-action 'pre-command-hook #'emacsclient-default-directory-reset-frame-parameter)

        (cl-labels ((hook-action (hook-var)
                      (emacsclient-default-directory-log "%S hook: %S" hook-action hook-var)
                      (funcall hook-action hook-var #'emacsclient-default-directory-reset-frame-parameter))
                    (advise (target-function)
                      (emacsclient-default-directory-log "%s -advice: %S" (if emacsclient-default-directory-mode "add" "remove") target-function)
                      (if emacsclient-default-directory-mode
                          (advice-add target-function :before #'emacsclient-default-directory-reset-frame-parameter)
                        (advice-remove target-function #'emacsclient-default-directory-reset-frame-parameter))))
          ;; reset frame parameter before stuff that loads things
          (cl-loop
           for hook-var in emacsclient-default-directory-reset-hooks
           do
           (if (boundp hook-var)
               (hook-action hook-var)
             (emacsclient-default-directory-log "%S not bound" hook-var)
             (with-eval-after-load (symbol-file hook-var 'defvar)
               (hook-action hook-var))))

          ;; also do this for functions that don't have hooks
          (cl-loop
           for target-function in emacsclient-default-directory-reset-functions
           do
           (if (fboundp target-function)
               (advise target-function)
             (emacsclient-default-directory-log "%S not bound" target-function)
             (with-eval-after-load (symbol-file target-function 'defun)
               (advise target-function))
             ))))
        )))

(provide 'emacsclient-default-directory)

;;; emacsclient-default-directory.el ends here
