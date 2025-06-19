;;; dired-sort.el

;; Copyright (C) 2025 Mykhailo Kazarian

;; Author: Your Name <michael.kazarian@gmail.com>
;; Version: 0.1
;; Keywords: dired, convenience
;; Package-Requires: ((emacs "24.4"))
;; URL: https://your.repo.url/here
;; License: GPL-3+

;;; Commentary:

;; This minor mode provides a convenient way to toggle the visibility of hidden
;; files (dotfiles) in Dired buffers, as well as flexible sorting options.
;;
;; Features:
;; - Toggle showing/hiding hidden files (dotfiles) with automatic insertion of `..` when hidden files are off.
;; - Sort Dired buffers by:
;;     - Name (normal and reverse)
;;     - Modification date (normal and reverse)
;;     - File extension (normal and reverse)
;; - Sorting respects the hidden files visibility state.
;; - Uses `dired-sort-extra-switches` to manage sorting flags separately.
;; - Automatically updates Dired buffers on buffer/window changes.
;;
;; Usage:
;;
;; 1. Place this file in your load-path and add to your init file:
;;
;;    (require 'dired-sort)
;;    (dired-sort-mode 1)
;;
;; 2. Toggle hidden files visibility with:
;;
;;    M-x dired-sort-toggle-hidden
;;
;; 3. Sort using commands:
;;
;;    - dired-sort-by-name            (sort by name)
;;    - dired-sort-by-name-reverse    (sort by name reverse)
;;    - dired-sort-by-date            (sort by modification date)
;;    - dired-sort-by-date-reverse    (sort by modification date reverse)
;;    - dired-sort-by-extension       (sort by extension)
;;    - dired-sort-by-extension-reverse (sort by extension reverse)
;;
;; 4. Bind keys as desired, e.g.:
;;
;;    (with-eval-after-load 'dired
;;      (define-key dired-mode-map (kbd "C-c h") #'dired-sort-toggle-hidden)
;;      ;; Example prefix map:
;;      (define-prefix-command 'dired-sort-map)
;;      (define-key dired-mode-map (kbd "ESC ESC") 'dired-sort-map)
;;      (define-key dired-sort-map (kbd "d") #'dired-sort-by-date)
;;      (define-key dired-sort-map (kbd "d r") #'dired-sort-by-date-reverse)
;;      (define-key dired-sort-map (kbd "n") #'dired-sort-by-name)
;;      (define-key dired-sort-map (kbd "n r") #'dired-sort-by-name-reverse)
;;      (define-key dired-sort-map (kbd "e") #'dired-sort-by-extension)
;;      (define-key dired-sort-map (kbd "e r") #'dired-sort-by-extension-reverse))
;;
;; 5. After invoking a sort command, the Dired buffer refreshes automatically.

;;; Code:

(defgroup dired-sort nil
  "Toggle display of hidden files in Dired."
  :group 'dired)

(defcustom dired-sort-show-hidden nil
  "Whether to show hidden files in Dired.
This variable is buffer-local in Dired buffers."
  :type 'boolean
  :group 'dired-sort)

(defvar-local dired-sort-extra-switches ""
  "Extra switches like -t or -r used for sorting in Dired.")

(defun dired-sort--insert-dot-dot ()
  "Insert `ls -ld ..` output in Dired when hidden files are off."
  (when (and ;(not dired-sort-show-hidden)
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
  (setq dired-listing-switches
        (format "-Alh --group-directories-first %s" dired-sort-extra-switches))
  ;; (message "Hidden files: ON")
  (dired-sort-other dired-listing-switches)
  (dired-sort--insert-dot-dot))

(defun dired-sort-hide-hidden-files ()
  "Disable showing hidden files in Dired and insert `..` manually."
  (interactive)
  (setq dired-listing-switches
        (format "-lh --group-directories-first %s" dired-sort-extra-switches))
  ;; (message "Hidden files: OFF")
  (dired-sort-other dired-listing-switches)
  (dired-sort--insert-dot-dot))

(defun dired-sort-toggle-hidden ()
  "Toggle visibility of hidden files in Dired."
  (interactive)
  (setq dired-sort-show-hidden (not dired-sort-show-hidden))
  (if dired-sort-show-hidden
      (dired-sort-show-hidden-files)
    (dired-sort-hide-hidden-files))
  (dired-sort--setup))

(defun dired-sort-by-name ()
  "Sort by name."
  (interactive)
  (setq dired-sort-extra-switches "")
  (dired-sort--setup))

(defun dired-sort-by-name-reverse ()
  "Sort by name in reverse."
  (interactive)
  (setq dired-sort-extra-switches "-r")
  (dired-sort--setup))

(defun dired-sort-by-extension ()
  "Sort by file extension."
  (interactive)
  (setq dired-sort-extra-switches "-X")
  (dired-sort--setup))

(defun dired-sort-by-extension-reverse ()
  "Sort by file extension, reverse."
  (interactive)
  (setq dired-sort-extra-switches "-X -r")
  (dired-sort--setup))


(defun dired-sort-by-date ()
  "Sort by modification time (oldest first)."
  (interactive)
  (setq dired-sort-extra-switches "-t -r")
  (dired-sort--setup))

(defun dired-sort-by-date-reverse ()
  "Sort by modification time, reverse (newest first)."
  (interactive)
  (setq dired-sort-extra-switches "-t")
  (dired-sort--setup))

(defvar-local dired-sort--in-progress nil
  "Prevent recursion during setup.")

(defun dired-sort--setup ()
  "Apply listing switches based on `dired-sort-show-hidden`."
  (interactive)
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
  (define-key dired-mode-map (kbd "M-g n") #'dired-sort-by-name)
  (define-key dired-mode-map (kbd "M-g r n") #'dired-sort-by-name-reverse)
  (define-key dired-mode-map (kbd "M-g d") #'dired-sort-by-date)
  (define-key dired-mode-map (kbd "M-g r d") #'dired-sort-by-date-reverse)
  (define-key dired-mode-map (kbd "M-g x") #'dired-sort-by-extension)
  (define-key dired-mode-map (kbd "M-g r x") #'dired-sort-by-extension-reverse)
  (define-key dired-mode-map (kbd "M-g h") #'dired-sort-toggle-hidden))

(provide 'dired-sort)

;;; dired-sort.el ends here
