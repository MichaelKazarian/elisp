;;; emacs-rc-markdown.el --- 

;; Copyright (C) Michael Kazarian
;;
;; Author: Michael Kazarian <michael.kazarian@gmail.com>
;; Keywords: 
;; Requirements: 
;; Status: not intended to be distributed yet

(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(custom-set-variables
 '(markdown-command "/usr/bin/pandoc")
 )
(eval-after-load "markdown-mode"
  '(defalias 'markdown-add-xhtml-header-and-footer 'as/markdown-add-xhtml-header-and-footer))

(defun as/markdown-add-xhtml-header-and-footer (title)
    "Wrap XHTML header and footer with given TITLE around current buffer."
    (goto-char (point-min))
    (insert "<!DOCTYPE html5>\n"
	    "<html>\n"
	    "<head>\n<title>")
    (insert title)
    (insert "</title>\n")
    (insert "<meta charset=\"utf-8\" />\n")
    (when (> (length markdown-css-paths) 0)
      (insert (mapconcat 'markdown-stylesheet-link-string markdown-css-paths "\n")))
    (insert "\n</head>\n\n"
	    "<body>\n\n")
    (goto-char (point-max))
    (insert "\n"
	    "</body>\n"
	    "</html>\n"))

(add-hook 'markdown-mode-hook
          (lambda ()
            (local-set-key "\C-c;" 'comment-or-uncomment-region)
            (electric-pair-mode)
            (outline-minor-mode 1)
            (linum-mode 1)
            (whitespace-mode 1)
            ))

;;; emacs-rc-markdown.el ends here
