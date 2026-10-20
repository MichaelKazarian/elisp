;;; emacs-rc-markdown.el --- Markdown mode settings -*- lexical-binding: t; -*-

;; Copyright (C) Michael Kazarian
;;
;; Author: Michael Kazarian <michael.kazarian@gmail.com>
;; Status: not intended to be distributed yet

(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(setq markdown-command (or (executable-find "pandoc") "pandoc"))

(defun as/markdown-add-html-header-and-footer (title)
  "Wrap HTML5 header and footer with given TITLE around current buffer."
  (goto-char (point-min))
  (insert "<!DOCTYPE html>\n"
          "<html lang=\"en\">\n"
          "<head>\n<title>")
  (insert title)
  (insert "</title>\n"
          "<meta charset=\"utf-8\">\n")
  (when (> (length markdown-css-paths) 0)
    (insert (mapconcat #'markdown-stylesheet-link-string
                       markdown-css-paths "\n")))
  (insert "\n</head>\n\n<body>\n\n")
  (goto-char (point-max))
  (insert "\n</body>\n</html>\n"))

(with-eval-after-load 'markdown-mode
  (defalias 'markdown-add-xhtml-header-and-footer
    #'as/markdown-add-html-header-and-footer))

(add-hook 'markdown-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c ;") #'comment-or-uncomment-region)
            (electric-pair-local-mode 1)
            (outline-minor-mode 1)
            (display-line-numbers-mode 1)
            (whitespace-mode 1)))

;;; emacs-rc-markdown.el ends here
