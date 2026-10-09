(define-derived-mode pdf-text-mode text-mode "PDF-txt"
  "Показує PDF як текст через pdftotext."
  (let ((inhibit-read-only t) (f buffer-file-name))
    (erase-buffer)
    (call-process "pdftotext" nil t nil "-layout" f "-")
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)))
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-text-mode))

