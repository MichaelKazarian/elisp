;;; rc-dired.el --- 

;; Copyright (C) Michael Kazarian
;;
;; Author: Michael Kazarian <michael.kazarian@gmail.com>

(require 'dired-lister)
(dired-lister-mode 1) ; Enable the mode globally

(defvar my-dired-keybindings
  '(("C-c h" "Toggle hidden files")
    ("C-c a" "Alphabetically")
    ("C-c d" "Date (newest first)")
    ("C-c r" "Date (oldest first)")
    ("C-c s" "Show sort menu"))
  "List of Dired keybindings as ((key) (description)), matching sort-options names.")

;; Базові налаштування Dired
(setq dired-listing-switches "-lh --group-directories-first")

;; Змінна для відстеження стану прихованих файлів
(defvar my-dired-show-hidden nil
  "Whether to show hidden files in Dired (controls -a in ls switches).")

(defun my-dired-insert-dot-dot ()
  "Вставляє результат ls -ld .. у Dired, якщо приховані файли вимкнені."  
  (when (and (not my-dired-show-hidden)
             (eq major-mode 'dired-mode))
    (let ((inhibit-read-only t)
          (dotdot (with-temp-buffer
                    (when (eq 0 (call-process "ls" nil t nil "-ld" ".."))
                      (buffer-string)))))
      (goto-char (point-min))
      (forward-line 2) ;; Знайти місце після заголовка
      (message "DOT")
      (insert dotdot))))

(defun my-dired-show-hidden-files ()
  "Enable showing hidden files in Dired."
  (interactive)
  (setq dired-listing-switches "-alh --group-directories-first")
  (message "Hidden files: ON")
  (dired-sort-other dired-listing-switches))

(defun my-dired-hide-hidden-files ()
  "Disable showing hidden files in Dired and insert .. manually."
  (interactive)
  (setq dired-listing-switches "-lh --group-directories-first")
  (message "Hidden files: OFF")
  (dired-sort-other dired-listing-switches)
  (my-dired-insert-dot-dot))

(defun my-dired-toggle-hidden ()
  "Toggle showing hidden files in Dired by modifying dired-listing-switches."
  (interactive)
  (setq my-dired-show-hidden (not my-dired-show-hidden))
  (if my-dired-show-hidden
      (my-dired-show-hidden-files)
    (my-dired-hide-hidden-files))
  (revert-buffer))

(defvar my-dired-setup-in-progress nil
  "Non-nil if `my-dired-setup-hook` is currently running to avoid recursion.")

(defun my-dired-setup-hook ()
  "Apply listing switches based on `my-dired-show-hidden`."
  (interactive)
  (unless my-dired-setup-in-progress
    (let ((my-dired-setup-in-progress t))  ;; локально
  (message "Setup state %s " my-dired-show-hidden)
  (if my-dired-show-hidden
      (my-dired-show-hidden-files)
    (my-dired-hide-hidden-files)))))

(defun my-dired-buffer-list-update-hook ()
  (when (eq major-mode 'dired-mode)
    (my-dired-setup-hook)))

(add-hook 'buffer-list-update-hook #'my-dired-buffer-list-update-hook)

;; Функції для сортування за датою
(defun my-dired-sort-by-date ()
  "Sort Dired by date, keeping directories first, respecting hidden files state."
  (interactive)
  (let ((switches (if my-dired-show-hidden
                      "-alh --group-directories-first -t"
                    "-lh --group-directories-first -t")))
    (dired-sort-other switches)))

(defun my-dired-sort-by-date-reverse ()
  "Sort Dired by date in reverse order, keeping directories first, respecting hidden files state."
  (interactive)
  (let ((switches (if my-dired-show-hidden
                      "-alh --group-directories-first -t -r"
                    "-lh --group-directories-first -t -r")))
    (dired-sort-other switches)))

(defun my-dired-sort-by-name ()
  "Sort Dired alphabetically, keeping directories first, respecting hidden files state."
  (interactive)
  (let ((switches (if my-dired-show-hidden
                      "-alh --group-directories-first"
                    "-lh --group-directories-first")))
    (dired-sort-other switches)))

(defun my-dired-sort-menu ()
  "Display a menu for sorting options in Dired with keybindings."
  (interactive)
  (let* ((sort-options
          '(("Alphabetically" . my-dired-sort-by-name)
            ("Date (newest first)" . my-dired-sort-by-date)
            ("Date (oldest first)" . my-dired-sort-by-date-reverse)))
         (options-with-index
          (cl-loop for opt in sort-options
                   for i from 1
                   collect (format "%d: %s (%s)"
                                   i
                                   (car opt)
                                   (or (car (rassoc (car opt) my-dired-keybindings)) "no key"))))
         (prompt (concat "Select sort option:\n"
                         (mapconcat 'identity options-with-index "\n")
                         "\nEnter number or option: "))
         (choice (completing-read prompt options-with-index nil t))
         (index (string-to-number (replace-regexp-in-string "^\\([0-9]+\\):.*" "\\1" choice))))
    (cond ((and (> index 0) (<= index (length sort-options)))
           (funcall (cdr (nth (1- index) sort-options))))
          (t
           (let* ((clean-choice (replace-regexp-in-string "^[0-9]+: \\(.*\\) (.*)$" "\\1" choice))
                  (selected (assoc clean-choice sort-options)))
             (if selected
                 (funcall (cdr selected))
               (message "Invalid selection: %s" choice)))))))

;; Гарячі клавіші
(define-key dired-mode-map (kbd "C-c h") 'my-dired-toggle-hidden)
(define-key dired-mode-map (kbd "C-c d") 'my-dired-sort-by-date)
(define-key dired-mode-map (kbd "C-c r") 'my-dired-sort-by-date-reverse)
(define-key dired-mode-map (kbd "C-c a") 'my-dired-sort-by-name)
(define-key dired-mode-map (kbd "C-c s") 'my-dired-sort-menu)

;;; rc-dired.el ends here
