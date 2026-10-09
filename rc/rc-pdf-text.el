;; -*- lexical-binding: t; -*-
;; Profile: reader. PDF as plain text (pdftotext -layout), re-filled to window width.

(require 'cl-lib)
(require 'seq)

(defvar my/pdf-code-indent 4
  "A line indented at least this many columns deeper than the page's
base indent is treated as code/formula/quote and is not re-filled.
Increase it if first lines of paragraphs get mistaken for code.")

(defvar-local pdf-text--paragraphs nil
  "Prepared text of the current PDF (prose unwrapped, code marked).")

(defun my/pdf-to-text (file)
  "Return text of PDF FILE (pdftotext -layout) as a decoded string."
  (unless (executable-find "pdftotext")
    (user-error "Install poppler (pdftotext)"))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let* ((coding-system-for-read 'binary)
           (status (call-process "pdftotext" nil t nil
                                 "-enc" "UTF-8" "-layout" file "-")))
      (unless (eq status 0)
        (error "pdftotext failed (exit %S)" status)))
    (decode-coding-string (buffer-string) 'utf-8)))

(defun my/pdf--indent (line)
  (or (string-match "[^ \t]" line) 0))

(defun my/pdf--base-indent (lines)
  "Most common indentation among non-blank LINES (the page's left margin)."
  (let ((h (make-hash-table)) (best 0) (best-n 0))
    (dolist (l lines)
      (unless (string-empty-p l)
        (cl-incf (gethash (my/pdf--indent l) h 0))))
    (maphash (lambda (k n) (when (> n best-n) (setq best k best-n n))) h)
    best))

(defun my/pdf-unwrap (text)
  "Prepare layout TEXT: join prose lines into paragraphs, keep code lines.
Code lines get the text property `pdf-pre'. Page breaks become blank lines."
  (let* ((pages (mapcar (lambda (p)
                          (mapcar #'string-trim-right (split-string p "\n")))
                        (split-string text "\f")))
         (lens (sort (mapcar #'length
                             (seq-remove #'string-empty-p (apply #'append pages)))
                     #'<))
         (ref (if lens (nth (floor (* 0.9 (1- (length lens)))) lens) 0))
         (threshold (* 0.75 ref))
         out)
    (cl-flet ((blank () (when (and out (not (equal (car out) "")))
                          (push "" out))))
      (dolist (lines pages)
        (let ((base (my/pdf--base-indent lines))
              cur last-len prev-code)
          (cl-flet ((flush () (when cur (push cur out) (setq cur nil))))
            (dolist (l lines)
              (let ((ind (my/pdf--indent l)))
                (cond
                 ((string-empty-p l)
                  (flush) (blank)
                  (setq prev-code nil last-len nil))
                 ;; code / formula / quote: deep indent, or short line
                 ;; right after code (closing brace, #define, ...)
                 ((or (>= ind (+ base my/pdf-code-indent))
                      (and prev-code (< (length l) threshold)))
                  (flush)
                  (setq prev-code t last-len nil)
                  (push (propertize
                         (concat (make-string (max 0 (- ind base)) ?\s)
                                 (string-trim-left l))
                         'pdf-pre t)
                        out))
                 ;; prose
                 (t
                  (setq prev-code nil)
                  (let ((s (string-trim-left l)))
                    (if (and cur last-len (>= last-len threshold))
                        (setq cur
                              (cond
                               ((string-suffix-p "\u00ad" cur)
                                (concat (substring cur 0 -1) s))
                               ((and (string-match-p "[[:alpha:]]-\\'" cur)
                                     (string-match-p "\\`[[:lower:]]" s))
                                (concat (substring cur 0 -1) s))
                               (t (concat cur " " s))))
                      (flush)
                      (setq cur s)))
                  (setq last-len (length l))))))
            (flush)
            (blank)))))              ; page break -> blank line
    (mapconcat #'identity (nreverse out) "\n")))

(defun pdf-text-refill ()
  "Re-fill prose to the window width; code lines stay as they are."
  (interactive)
  (let* ((inhibit-read-only t)
         (ratio (/ (float (point)) (max 1 (buffer-size))))
         (fill-column (max 20 (1- (window-body-width)))))
    (erase-buffer)
    (insert pdf-text--paragraphs)
    (goto-char (point-min))
    (while (not (eobp))
      (if (or (eolp) (get-text-property (point) 'pdf-pre))
          (forward-line 1)
        (let ((end (copy-marker (line-end-position))))
          (fill-region (point) end)
          (goto-char end)
          (forward-line 1))))
    (goto-char (floor (* ratio (buffer-size))))
    (set-buffer-modified-p nil)))

(defun pdf-text-quit ()
  "Close the PDF text buffer and leave its window."
  (interactive)
  (quit-window t))

(defvar pdf-text-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "g") #'pdf-text-refill)
    (define-key m (kbd "q") #'pdf-text-quit)
    m))

(define-derived-mode pdf-text-mode text-mode "PDF-txt"
  "Show a PDF as plain text, prose filled to the window width."
  (let ((file buffer-file-name)
        (inhibit-read-only t))
    (set-buffer-multibyte t)
    (setq buffer-file-coding-system 'utf-8)
    (setq pdf-text--paragraphs (my/pdf-unwrap (my/pdf-to-text file)))
    (visual-line-mode 1)             ; long code lines wrap visually
    (pdf-text-refill)
    (goto-char (point-min))
    (setq buffer-read-only t)))

(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-text-mode))
