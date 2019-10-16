;;; emacs-rc-java.el 

;; Copyright (C) 2009 Michael Kazarian
;; Author: michael.kazarian@gmail.com
;; Status: not intended to be distributed yet
;;
(defun my-java-mode-hook ()
  ;; (local-set-key (kbd "RET") 'newline-and-indent)
  (linum-mode 1)
  (setq tab-width 2)
  (hs-minor-mode)
  (setq c-basic-offset 2)
  (define-key c-mode-base-map "\C-m" 'c-context-line-break)
  (local-set-key (kbd "C-;") 'comment-or-uncomment-region)
  (local-set-key [f8] 'flycheck-mode)
  ;; Fix indentation for anonymous classes
  (c-set-offset 'substatement-open 0)
  (if (assoc 'inexpr-class c-offsets-alist)
      (c-set-offset 'inexpr-class 0))
  ;; Indent arguments on the next line as indented body.
  (c-set-offset 'arglist-intro '+)
  (yas-minor-mode))
(add-hook 'java-mode-hook 'my-java-mode-hook)

;;; meghanada
(add-hook 'java-mode-hook
          (lambda ()
            ;; meghanada-mode on
            (meghanada-mode t)
            (flycheck-mode +1)
            ;; use code format
            ;; (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)
            ))
(cond
   ((eq system-type 'windows-nt)
    (setq meghanada-java-path (expand-file-name "bin/java.exe" (getenv "JAVA_HOME")))
    (setq meghanada-maven-path "mvn.cmd"))
   (t
    (setq meghanada-java-path "java")
    (setq meghanada-maven-path "mvn")))
;;; meghanada

;;; emacs-rc-java.el ends here
