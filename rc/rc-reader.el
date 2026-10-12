;; -*- lexical-binding: t; -*-
;; Profile: reader. Start screen with recent documents (no extra packages).

(require 'recentf)
(require 'seq)

(defvar my/recent-regexp "\\.\\(epub\\|pdf\\|md\\|org\\|txt\\)\\'"
  "Only files matching this regexp are listed and remembered.")
(defvar my/recent-count 20
  "How many recent documents to show.")
(defvar my/recent-line-ratio 0.8
  "Width of the header line and of each entry, as a share of window width.")
(defvar my/recent-date-format "%d.%m.%Y %H:%M"
  "Format of the access date shown at the right of each entry.")
(defvar my/recent-gap 1
  "Blank lines between entries (bigger gap = easier to tap).")

(setq recentf-save-file (expand-file-name "recentf" user-emacs-directory)
      recentf-max-saved-items 100
      recentf-auto-cleanup 'never
      recentf-exclude (list (lambda (f) (not (string-match-p my/recent-regexp f))))
      inhibit-startup-screen t
      mouse-1-click-follows-link t)
(recentf-mode 1)

;; Termux can be killed without a clean exit: save the list when idle
(run-with-idle-timer 30 t #'recentf-save-list)

(define-button-type 'my-recent-button
  'face nil                 ; no underline: faces come from the text itself
  'follow-link t)

(defvar my/recent-mode-map
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m special-mode-map)          ; q closes the window
    (define-key m (kbd "g") #'my/recent-render)
    (define-key m (kbd "n") #'forward-button)
    (define-key m (kbd "p") #'backward-button)
    m))

(define-derived-mode my/recent-mode special-mode "Recent"
  "List of recently opened documents."
  (setq truncate-lines t))

(defun my/recent--center (str width)
  "Return STR padded on the left to be centered in WIDTH columns."
  (concat (make-string (max 0 (/ (- width (string-width str)) 2)) ?\s) str))

(defun my/recent--cut-dir (dir max-width)
  "Cut DIR (home already shown as ~) to MAX-WIDTH columns.
The tail of the path is kept: ~/…/last/dirs.  Used only when the
directory is on its own line and still too long."
  (if (<= (string-width dir) max-width)
      dir
    (let* ((home (and (string-prefix-p "~/" dir) "~/"))
           (prefix (concat (or home "") "…"))
           (keep (max 1 (- max-width (string-width prefix))))
           (tail (substring dir (max 0 (- (length dir) keep)))))
      (concat prefix tail))))

(defun my/recent--entries ()
  "Return (FILE . ACCESS-TIME) for recent documents, newest access first.
Entries that point to the same real file are listed once."
  (let ((seen (make-hash-table :test #'equal))
        entries)
    (dolist (f recentf-list)
      (let* ((f (expand-file-name f))
             (a (and (string-match-p my/recent-regexp f)
                     (file-attributes f)))) ; nil if file is gone
        (when a
          (let ((key (file-truename f)))
            (unless (gethash key seen)
              (puthash key t seen)
              (push (cons f (file-attribute-access-time a)) entries))))))
    (seq-take (sort entries (lambda (a b) (time-less-p (cdr b) (cdr a))))
              my/recent-count)))

(defun my/recent-render ()
  "Fill the current buffer: header, separator line and document entries."
  (interactive)
  (let* ((inhibit-read-only t)
         (win-w (window-body-width))
         (line-w (floor (* win-w my/recent-line-ratio)))
         (pad (make-string (max 0 (/ (- win-w line-w) 2)) ?\s))
         (entries (my/recent--entries)))
    (erase-buffer)
    (insert (propertize (my/recent--center "Welcome to Emacs" win-w) 'face 'bold)
            "\n")
    (insert (propertize
             (my/recent--center
              (if after-init-time
                  (format "Started in %s s" (emacs-init-time "%.2f"))
                "Starting...")
              win-w)
             'face 'shadow)
            "\n")
    (insert pad (propertize (make-string line-w ?─) 'face 'shadow) "\n\n")
    (if (null entries)
        (insert (my/recent--center "(nothing yet)" win-w) "\n")
      (dolist (e entries)
        (let* ((file (car e))
               (date (format-time-string my/recent-date-format (cdr e)))
               (name (file-name-nondirectory file))
               (dir (abbreviate-file-name (file-name-directory file))) ; ends with /
               ;; marker "▸ " (2) + at least 2 spaces before the date
               (avail (max 5 (- line-w 4 (string-width date))))
               (fits (<= (+ (string-width dir) (string-width name)) avail))
               (shown-dir (if fits dir ""))
               (shown-name (if fits
                               name
                             (truncate-string-to-width name avail nil nil "…")))
               (gap (max 2 (- line-w 2 (string-width shown-dir)
                              (string-width shown-name) (string-width date))))
               (beg (point)))
          (insert pad
                  (propertize "▸ " 'face 'shadow)
                  (propertize shown-dir 'face 'shadow)
                  (propertize shown-name 'face 'bold)
                  (make-string gap ?\s)
                  (propertize date 'face 'shadow))
          ;; path too long for the first line: show it on the next one
          (unless fits
            (insert "\n" pad "  "
                    (propertize (my/recent--cut-dir
                                 (directory-file-name dir) (- line-w 4))
                                'face 'shadow)))
          (make-text-button beg (point)
                            :type 'my-recent-button
                            'action (let ((f file)) (lambda (_) (find-file f)))
                            'help-echo file)
          (insert (make-string (1+ my/recent-gap) ?\n)))))
    (goto-char (point-min))
    (ignore-errors (forward-button 1))))

(defun my/recent-show ()
  "Show the recent documents buffer."
  (interactive)
  (let ((buf (get-buffer-create "*Recent*")))
    (with-current-buffer buf
      (my/recent-mode)
      (my/recent-render))
    (switch-to-buffer buf)))

(setq initial-buffer-choice
      (lambda ()
        (with-current-buffer (get-buffer-create "*Recent*")
          (my/recent-mode)
          (my/recent-render)
          (current-buffer))))

;; Re-center when the window size changes (e.g. phone rotated)
(add-hook 'window-size-change-functions
          (lambda (frame)
            (let ((w (get-buffer-window "*Recent*" frame)))
              (when w
                (with-selected-window w
                  (my/recent-render))))))


;;; RO
(defun my/doc-edit-p ()
  "Non-nil if this buffer visits a plain document that may be edited."
  (and buffer-file-name
       (not (derived-mode-p 'pdf-text-mode 'nov-mode))
       (string-match-p my/recent-regexp buffer-file-name)))

(defun my/doc-toggle-edit ()
  "Switch the buffer between read-only and editable."
  (interactive)
  (read-only-mode (if buffer-read-only -1 1))
  (force-mode-line-update))

;; e and q work as commands only while the buffer is read-only;
;; in an editable buffer the filter returns nil, so they just type letters.
(define-minor-mode my/doc-ro-mode
  "Reader keys for read-only documents: e = edit, q = close."
  :lighter " RO"
  :keymap
  `(keymap
    (?e menu-item "" my/doc-toggle-edit
        :filter ,(lambda (cmd) (and buffer-read-only cmd)))
    (?q menu-item "" my/doc-quit
        :filter ,(lambda (cmd) (and buffer-read-only cmd)))
    (,(kbd "C-c e") . my/doc-toggle-edit)))

(defun my/doc-lock ()
  "Open plain documents read-only."
  (when (my/doc-edit-p)
    (my/doc-ro-mode 1)
    (read-only-mode 1)))

;;; Touch

(defun my/doc-edit-button ()
  "Header button that shows the current state and toggles it."
  (propertize (if buffer-read-only " ✎ Edit " " ✔ Lock ")
              'face 'button 'mouse-face 'highlight
              'keymap (let ((m (make-sparse-keymap)))
                        (define-key m [header-line mouse-1]
                                    #'my/doc-toggle-edit)
                        m)))

(defun my/doc-quit ()
  "Close the current document buffer and leave its window."
  (interactive)
  (quit-window t))

(defun my/touch-header ()
  "Tappable header line: back to previous buffer / recent list / close."
  (setq header-line-format
        (list
         (propertize " ◀ Back " 'face 'button 'mouse-face 'highlight
                     'keymap (let ((m (make-sparse-keymap)))
                               (define-key m [header-line mouse-1]
                                           #'previous-buffer)
                               m))
         "  "
         (propertize " ☰ Recent " 'face 'button 'mouse-face 'highlight
                     'keymap (let ((m (make-sparse-keymap)))
                               (define-key m [header-line mouse-1]
                                           #'my/recent-show)
                               m))
         "  "
         '(:eval (when (bound-and-true-p my/doc-ro-mode)
                   (my/doc-edit-button)))
         "  "
         (propertize " ✕ Close " 'face 'button 'mouse-face 'highlight
                     'keymap (let ((m (make-sparse-keymap)))
                               (define-key m [header-line mouse-1]
                                           #'my/doc-quit)
                               m)))))


(defalias 'pdf-text-quit #'my/doc-quit)

;; (add-hook 'pdf-text-mode-hook #'my/touch-header)
(add-hook 'nov-mode-hook #'my/touch-header)
(add-hook 'text-mode-hook #'my/touch-header)
(add-hook 'org-mode-hook #'my/touch-header)
(add-hook 'find-file-hook #'my/doc-lock)

(global-set-key (kbd "C-c r") #'my/recent-show)
