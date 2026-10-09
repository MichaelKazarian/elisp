;; -*- lexical-binding: t; -*-
;; Profile: reader. Show PDF as plain text (no images).

(defun my/pdf-to-text (file)
  "Return plain text of PDF FILE as a decoded string.
Prefers pdftotext (with -layout), falls back to mutool.
Output is read as raw bytes and decoded explicitly as UTF-8."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let* ((coding-system-for-read 'binary)
           (status
            (cond
             ((executable-find "pdftotext")
              (call-process "pdftotext" nil t nil
                            "-enc" "UTF-8" "-layout" "-nopgbrk" file "-"))
             ((executable-find "mutool")
              (call-process "mutool" nil t nil
                            "draw" "-q" "-F" "txt" "-o" "-" file))
             (t (user-error "Install poppler or mupdf-tools")))))
      (unless (eq status 0)
        (error "PDF text extraction failed (exit %S)" status)))
    (decode-coding-string (buffer-string) 'utf-8)))

(define-derived-mode pdf-text-mode text-mode "PDF-txt"
  "Show a PDF as plain text."
  (let ((inhibit-read-only t)
        (text (my/pdf-to-text buffer-file-name)))
    (set-buffer-multibyte t)
    (setq buffer-file-coding-system 'utf-8)
    (erase-buffer)
    (insert text)
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)))

(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-text-mode))
