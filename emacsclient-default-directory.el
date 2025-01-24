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

(defun emacsclient-default-directory-find-file-hook ()
  "Check if the frame parameter `emacsclient-default-directory' is set.
If so, set `default-directory' buffer-locally and reset the frame parameter."
  (let* ((frame (selected-frame))
         (dir (frame-parameter frame 'emacsclient-default-directory)))
    (message "emacsclient-default-directory-find-file-hook: %s %s" dir default-directory)
    (when dir
      (setq-local default-directory dir))
    (run-at-time emacsclient-default-directory-reset-timeout nil
                 (lambda ()
                   (message "Resetting emacsclient-default-directory frame parameter")
                   (set-frame-parameter frame 'emacsclient-default-directory nil)))))

(defun emacsclient-default-directory-setup ()
  "Add `emacsclient-default-directory-find-file-hook' to `find-file-hook'."
  (add-hook 'find-file-hook 'emacsclient-default-directory-find-file-hook))

(provide 'emacsclient-default-directory)

;;; emacsclient-default-directory.el ends here
