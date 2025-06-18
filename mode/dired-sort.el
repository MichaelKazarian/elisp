;;; dired-sort.el --- Toggle hidden files and add `..` in Dired -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Mykhailo Kazarian

;; Author: Your Name <your@email.com>
;; Version: 0.1
;; Keywords: dired, convenience
;; Package-Requires: ((emacs "24.4"))
;; URL: https://your.repo.url/here
;; License: GPL-3+

;;; Commentary:

;; This minor mode provides a convenient way to toggle the visibility of hidden
;; files (dotfiles) in Dired buffers. It also ensures that the `..` parent
;; directory entry is manually inserted when hidden files are turned off.

;; Features:
;; - Toggle between showing/hiding hidden files (dotfiles).
;; - Automatically inserts `..` when hidden files are not shown.
;; - Tracks global state with `dired-sort-show-hidden`.
;; - Automatically updates Dired view on buffer/window changes.

;; Usage:
;;
;; 1. Place this file in your load-path and add to your init file:
;;
;;    (require 'dired-sort)
;;    (dired-sort-mode 1)
;;
;; 2. Use the command `M-x dired-sort-toggle-hidden` to toggle visibility
;;    of hidden files.
;;
;; 3. You can bind it to a key in Dired:
;;
;;    (with-eval-after-load 'dired
;;      (define-key dired-mode-map (kbd "H") #'dired-sort-toggle-hidden))

;;; Code:

(defgroup dired-sort nil
  "Toggle display of hidden files in Dired."
  :group 'dired)

(defcustom dired-sort-show-hidden nil
  "Whether to show hidden files in Dired.
This variable is buffer-local in Dired buffers."
  :type 'boolean
  :group 'dired-sort)

(defun dired-sort--insert-dot-dot ()
  "Insert `ls -ld ..` output in Dired when hidden files are off."
  (when (and (not dired-sort-show-hidden)
             (eq major-mode 'dired-mode))
    (let ((inhibit-read-only t)
          (dotdot (with-temp-buffer
                    (when (eq 0 (call-process "ls" nil t nil "-ld" ".."))
                      (buffer-string)))))
      (goto-char (point-min))
      (forward-line 2)
      (insert dotdot))))

(defun dired-sort-show-hidden-files ()
  "Enable showing hidden files in Dired."
  (interactive)
  (setq dired-listing-switches "-alh --group-directories-first")
  (message "Hidden files: ON")
  (dired-sort-other dired-listing-switches))

(defun dired-sort-hide-hidden-files ()
  "Disable showing hidden files in Dired and insert `..` manually."
  (interactive)
  (setq dired-listing-switches "-lh --group-directories-first")
  (message "Hidden files: OFF")
  (dired-sort-other dired-listing-switches)
  (dired-sort--insert-dot-dot))

(defun dired-sort-toggle-hidden ()
  "Toggle visibility of hidden files in Dired."
  (interactive)
  (setq dired-sort-show-hidden (not dired-sort-show-hidden))
  (if dired-sort-show-hidden
      (dired-sort-show-hidden-files)
    (dired-sort-hide-hidden-files))
  (revert-buffer))

(defvar-local dired-sort--in-progress nil
  "Prevent recursion during setup.")

(defun dired-sort--setup ()
  "Apply listing switches based on `dired-sort-show-hidden`."
  (unless dired-sort--in-progress
    (let ((dired-sort--in-progress t))
      (if dired-sort-show-hidden
          (dired-sort-show-hidden-files)
        (dired-sort-hide-hidden-files)))))

(defun dired-sort--maybe-setup ()
  "Run setup if current buffer is Dired."
  (when (eq major-mode 'dired-mode)
    (dired-sort--setup)))

;;;###autoload
(define-minor-mode dired-sort-mode
  "Minor mode to toggle hidden files visibility in Dired."
  :global t
  :group 'dired-sort
  :lighter " DSort"
  (if dired-sort-mode
      (add-hook 'buffer-list-update-hook #'dired-sort--maybe-setup)
    (remove-hook 'buffer-list-update-hook #'dired-sort--maybe-setup)))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-c h") #'dired-sort-toggle-hidden))

(provide 'dired-sort)

;;; dired-sort.el ends here
