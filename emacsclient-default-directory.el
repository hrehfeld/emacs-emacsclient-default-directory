;;; emacsclient-default-directory.el --- Set default directory for emacsclient buffers -*- lexical-binding: t; -*-

;; Author: Hauke Rehfeld <emacs@haukerehfeld.de>
;; URL: https://github.com/hrehfeld/emacs-emacsclient-default-directory
;; Version: 0.1
;; Package-Requires: ((emacs "24.3"))
;; Keywords: convenience

;;; Commentary:

;; This package provides functions to manage default directories for
;; emacsclient buffers using frame parameters.

;;; Code:

(defvar emacsclient-default-directory-reset-timeout 5
  "Time in seconds after which the frame parameter `emacsclient-default-directory' is reset.")

(defvar emacsclient-default-directory-reset-timer nil "Timer that will unset the frame parameter that holds last `default-directory'.")

(defcustom emacsclient-default-directory-reset-hooks '(easysession-before-load-hook)
  "List of hooks that should reset the frame parameter `emacsclient-default-directory'.

These are usually hooks that are run before loading files unrelated to the last emacsclient command."
  :type '(repeat symbol)
  :group 'emacsclient-default-directory)

(defcustom emacsclient-default-directory-reset-functions '(activities-resume activities-switch)
  "List of functions that first should reset the frame parameter `emacsclient-default-directory'.")

(defun emacsclient-default-directory-reset-frame-parameter (&rest args)
  (message "Resetting emacsclient-default-directory frame parameter")
  (let ((frame (selected-frame)))
    (when (frame-live-p frame)
      (set-frame-parameter frame 'emacsclient-default-directory nil)))
  (when emacsclient-default-directory-reset-timer
    (cancel-timer emacsclient-default-directory-reset-timer)
    (setq emacsclient-default-directory-reset-timer nil)))

(defun emacsclient-default-directory-find-file-hook ()
  "Check if the frame parameter `emacsclient-default-directory' is set.
If so, set `default-directory' buffer-locally and reset the frame parameter."
  (let* ((frame (selected-frame))
         (dir (frame-parameter frame 'emacsclient-default-directory)))
    (message "emacsclient-default-directory-find-file-hook: %s %s" dir default-directory)
    (when dir
      (setq-local default-directory dir))
    (setq emacsclient-default-directory-reset-timer
          (run-at-time emacsclient-default-directory-reset-timeout nil #'emacsclient-default-directory-reset-frame-parameter))
    ))

(define-minor-mode emacsclient-default-directory-mode
  "Add `emacsclient-default-directory-find-file-hook' to `find-file-hook'."
  :global t
  :lighter ""
  (let ((hook-action (if emacsclient-default-directory-mode #'add-hook #'remove-hook)))
    (funcall hook-action
             'find-file-hook
             'emacsclient-default-directory-find-file-hook)

    ;; reset frame paramater before stuff that loads things
    (cl-loop
     for hook-var in emacsclient-default-directory-reset-hooks
     do
     (if (boundp hook-var)
         (funcall hook-action hook-var #'emacsclient-default-directory-reset-frame-parameter)
       (message "emacsclient-default-directory-mode: %S not bound" hook-var)))

    ;; also do this for functions that don't have hooks
    (cl-loop
     for target-function in emacsclient-default-directory-reset-functions
     do
     (if (fboundp target-function)
         (if emacsclient-default-directory-mode
           (advice-add target-function :before #'emacsclient-default-directory-reset-frame-parameter)
           (advice-remove target-function #'emacsclient-default-directory-reset-frame-parameter))
       (message "emacsclient-default-directory-mode: %S not bound" target-function)))))

(provide 'emacsclient-default-directory)

;;; emacsclient-default-directory.el ends here
