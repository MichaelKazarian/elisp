;;; rc-dired.el --- 

;; Copyright (C) Michael Kazarian
;;
;; Author: Michael Kazarian <michael.kazarian@gmail.com>
;; Keywords: 
;; Requirements: 
;; Status: not intended to be distributed yet

(defcustom dired-preview-auto-focus nil
  "If non-nil, automatically focus the preview window after opening it."
  :type 'boolean
  :group 'dired-preview)

(defcustom dired-preview-window-side 'right
  "Side where the preview window is displayed.
Valid values are `right', `left', `top', or `bottom'."
  :type '(choice (const right)
                 (const left)
                 (const top)
                 (const bottom))
  :group 'dired-preview)

(defcustom dired-preview-file-key (kbd "TAB")
  "Keybinding to open the file preview in Dired."
  :type 'key-sequence
  :group 'dired-preview)

(defcustom dired-preview-quit-dired-key (kbd "q")
  "Keybinding to quit Dired and close the preview."
  :type 'key-sequence
  :group 'dired-preview)

(defcustom dired-preview-quit-key (kbd "q")
  "Keybinding to close the preview window."
  :type 'key-sequence
  :group 'dired-preview)

(defcustom dired-preview-edit-key (kbd "e")
  "Keybinding to edit the previewed file."
  :type 'key-sequence
  :group 'dired-preview)

(defvar dired-preview-buffer-name "*dired-preview*"
  "Name of the dired preview buffer.")

(defun dired-preview--buffer ()
  "Return the preview buffer if it exists."
  (get-buffer dired-preview-buffer-name))

(defun dired-preview--window ()
  "Return the window showing the preview buffer, if any."
  (get-buffer-window (dired-preview--buffer) t))

(defvar-local previewed-file nil)
(defvar-local previewed-dired-window nil)

(defun dired-preview-close-previous ()
  "Close the previous preview window if it exists."
  (let ((win (ignore-errors (dired-preview--window))))
    (when (and win (windowp win) (window-live-p win))
      ;; (message "Closing previous preview window: %s" win)
      (quit-window nil win))))

(defun dired-preview-prepare-buffer (file buf)
  "Prepare the preview buffer BUF with contents of FILE."
  (with-current-buffer buf
    (let ((inhibit-read-only t))
      (erase-buffer)
      (condition-case err
          (insert-file-contents file)
        (error (message "Error inserting file contents: %s" err)))
      (let ((buffer-file-name file))
        (condition-case err
            (set-auto-mode)
          (error (message "Error in set-auto-mode: %s" err)))
        ;; (message "Major mode: %s" major-mode)
        )
      (font-lock-mode 1)
      (font-lock-flush)
      (font-lock-ensure)
      (read-only-mode 1)
      (setq-local previewed-file file)
      (setq-local previewed-dired-window (get-mru-window nil t t)))))

(defun dired-preview-disable-hooks-and-modes ()
  "Disable specific hooks and modes for the preview buffer."
  (let ((hooks-and-modes
         '((org-mode-hook . nil)
           (org-startup-folded . nil)
           (c-mode-hook . nil)
           (python-mode-hook . nil)
           (after-change-major-mode-hook . nil))))
    (dolist (hook-and-value hooks-and-modes)
      (set (car hook-and-value) (cdr hook-and-value))))
  (when (fboundp 'company-mode)
    (company-mode -1)))

(defun dired-preview-display-buffer (buf)
  "Display the preview buffer BUF in a side window.
Note: `display-buffer' can be replaced with `pop-to-buffer' to automatically
focus the window, but this will override `dired-preview-auto-focus' and
`save-selected-window' behavior."
  (let ((buffer-list-update-hook nil)
        (window-configuration-change-hook nil)
        (display-buffer-alist nil))
    (save-selected-window
      (let ((window (display-buffer
                     buf
                     `((display-buffer-in-side-window)
                       (side . ,dired-preview-window-side)
                       (window-width . 0.5)
                       (slot . 1)
                       (window-parameters (no-delete-other-windows . t)
                                          (dedicated . t)
                                          (dired-preview-just-created . t))))))
        (when window
          (set-window-dedicated-p window t)
          (when dired-preview-auto-focus
            (select-window window))
          ;; (message "Window created: %s, Buffer: %s, Visible: %s, Just-created: %s, Auto-focus: %s, Side: %s"
          ;;          window (buffer-name buf) (window-live-p window)
          ;;          (window-parameter window 'dired-preview-just-created)
          ;;          dired-preview-auto-focus
          ;;          dired-preview-window-side)
          )
        window))))

(defun dired-preview-setup-keybindings ()
  "Set up keybindings for the preview buffer."
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map (current-local-map))
    (when dired-preview-quit-key
      (define-key map dired-preview-quit-key #'dired-preview-quit))
    (when dired-preview-edit-key
      (define-key map dired-preview-edit-key #'dired-preview-edit-file))
    (use-local-map map)))

(defun dired-preview-file ()
  "Preview the file at point in a right-side window in read-only mode."
  (interactive)
  (let* ((file (dired-get-file-for-visit))
         (buf (get-buffer-create dired-preview-buffer-name)))
    (when (file-regular-p file)
      (dired-preview-close-previous)
      (dired-preview-prepare-buffer file buf)
      (with-current-buffer buf
        (dired-preview-disable-hooks-and-modes))
      (dired-preview-display-buffer buf)
      (with-selected-window (get-buffer-window buf t)
        (dired-preview-setup-keybindings)))))

(defun dired-preview-edit-file ()
  "Open the previewed file in a new editable buffer."
  (interactive)
  (when (bound-and-true-p previewed-file)
    (find-file previewed-file)))

(defun dired-preview-cleanup ()
  "Hide preview buffer and close its window if possible."
  (run-with-idle-timer
   0.3 nil ; Збільшена затримка
   (lambda ()
     (let ((buf (dired-preview--buffer))
           (win (dired-preview--window)))
       (when (and buf (buffer-live-p buf)
                  (not (eq (current-buffer) buf))
                  win (window-live-p win)
                  (not (window-parameter win 'dired-preview-just-created)))
         ;; (message "Cleanup: Closing window %s for buffer %s, Just-created: %s"
         ;;          win buf (window-parameter win 'dired-preview-just-created))
         (with-selected-window win
           (quit-window nil win)))))))

(defun dired-preview-quit ()
  "Close preview and return focus to Dired window."
  (interactive)
  (let ((win (selected-window)))
    ;; (message "Quitting window: %s, Visible: %s" win (window-live-p win))
    (quit-window t win)
    (when (and previewed-dired-window (windowp previewed-dired-window) (window-live-p previewed-dired-window))
      (select-window previewed-dired-window))))

(defun dired-preview-close-on-dired-exit ()
  "Close the preview window when exiting a Dired buffer."
  (when (eq major-mode 'dired-mode)
    (let ((buf (dired-preview--buffer))
          (win (dired-preview--window)))
      (when (and buf (buffer-live-p buf) win (window-live-p win))
        (message "Closing preview window %s for buffer %s on Dired exit"
                 win buf)
        (with-selected-window win
          (quit-window nil win))))))

(defun dired-preview-quit-dired ()
  "Quit Dired and close the preview window."
  (interactive)
  (dired-preview-close-on-dired-exit)
  (quit-window))

(with-eval-after-load 'dired
  (when dired-preview-file-key
    (define-key dired-mode-map dired-preview-file-key #'dired-preview-file))
  (when dired-preview-quit-dired-key
    (define-key dired-mode-map dired-preview-quit-dired-key #'dired-preview-quit-dired))
  (add-hook 'kill-buffer-hook #'dired-preview-close-on-dired-exit))

(add-hook 'buffer-list-update-hook #'dired-preview-cleanup)

;;; rc-dired.el ends here
