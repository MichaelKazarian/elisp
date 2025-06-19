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

(defconst dired-sort--commands-map
  '((dired-sort-by-name               "M-g n"   "Sort by name.")
    (dired-sort-by-name-reverse      "M-g r n" "Sort by name (reverse).")
    (dired-sort-by-date              "M-g d"   "Sort by date.")
    (dired-sort-by-date-reverse      "M-g r d" "Sort by date (reverse).")
    (dired-sort-by-extension         "M-g x"   "Sort by extension.")
    (dired-sort-by-extension-reverse "M-g r x" "Sort by extension (reverse).")
    (dired-sort-toggle-hidden        "M-g h"   "Toggle hidden files.")
    (dired-sort-show-menu            "C-c C-s" "Show sort command menu.")))

(defun dired-sort--active-p (fn)
  "Return non-nil if FN represents current sort state."
  (cond
   ((eq fn 'dired-sort-by-name)
    (string= dired-sort-extra-switches ""))
   ((eq fn 'dired-sort-by-name-reverse)
    (string= dired-sort-extra-switches "-r"))
   ((eq fn 'dired-sort-by-date)
    (string= dired-sort-extra-switches "-t -r"))
   ((eq fn 'dired-sort-by-date-reverse)
    (string= dired-sort-extra-switches "-t"))
   ((eq fn 'dired-sort-by-extension)
    (string= dired-sort-extra-switches "-X"))
   ((eq fn 'dired-sort-by-extension-reverse)
    (string= dired-sort-extra-switches "-X -r"))
   ((eq fn 'dired-sort-toggle-hidden)
    dired-sort-show-hidden)
   (t nil)))

(defun dired-sort--format-menu-line (index fn desc num-width desc-width key)
  "Format one menu line for FN with given widths."
  (let* ((active (dired-sort--active-p fn))
         (marker (if active "*" " ")))
    (format (format "%%%dd  [%%s] %%-%ds  %%s"
                    num-width desc-width)
            index marker desc key)))

(defun dired-sort--build-menu-lines ()
  "Build aligned menu lines for `dired-sort-show-menu`, using actual key bindings."
  (let* ((indexed
          (cl-mapcar (lambda (entry index)
                       (list index entry))
                     dired-sort--commands-map
                     (number-sequence 1 (length dired-sort--commands-map))))
         (num-width (length (number-to-string (length indexed))))
         (desc-width
          (apply #'max (mapcar (lambda (item)
                                 (length (nth 2 (cadr item))))
                               indexed))))
    (mapcar
     (lambda (item)
       (let* ((index (car item))
              (entry (cadr item))
              (fn (nth 0 entry))
              (desc (nth 2 entry))
              (key (substitute-command-keys (format "\\[%s]" fn))))
         (dired-sort--format-menu-line index fn desc num-width desc-width key)))
     indexed)))

(defun dired-sort-show-menu ()
  "Show a clean, aligned numbered menu of Dired sort commands and execute selected one."
  (interactive)
  (let* ((menu-lines (dired-sort--build-menu-lines))
         (prompt (concat "Choose sort option:\n"
                         (string-join menu-lines "\n")
                         "\nEnter number: "))
         (choice (read-number prompt))
         (selected (nth (1- choice) dired-sort--commands-map)))
    (if selected
        (call-interactively (nth 0 selected))
      (message "Invalid selection"))))

(defun dired-sort-setup-keys ()
  "Bind keys from `dired-sort--commands-map` in `dired-mode-map`."
  (dolist (entry dired-sort--commands-map)
    (let ((fn (nth 0 entry))
          (key (nth 1 entry)))
      (when (and fn key)
        (define-key dired-mode-map (kbd key) fn)))))

(with-eval-after-load 'dired
  (dired-sort-setup-keys)
   (define-key dired-mode-map (kbd "C-c m") #'dired-sort-show-menu))

(provide 'dired-sort)

;;; dired-sort.el ends here
