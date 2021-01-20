;;; emacs-rc-nxml.el --- 

;; Copyright (C) Michael Kazarian
;;
;; Author: Michael Kazarian <michael.kazarian@gmail.com>
;; Keywords: 
;; Requirements: 
;; Status: not intended to be distributed yet

(add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (highlight-symbol-mode nil) ;; Perhaps highlight-thing.el will better?
  (highlight-symbol-nav-mode nil)
  
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  ;; (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-enable-current-element-highlight t)
  (set (make-local-variable 'company-backends)
       '(company-css company-web-html company-yasnippet company-files))
  (emmet-mode 1)
  (local-set-key (kbd "RET") 'newline-and-indent)
  (local-set-key (kbd "C-;") 'comment-or-uncomment-region)
  (hs-minor-mode 1)
  (setq tab-width 2)
  (set 'ecb-layout-name"left5")
  ;; (ecb-layout-function-
  ; (linum-mode 1)
  (display-line-numbers-mode))

(add-hook 'web-mode-hook  'my-web-mode-hook)

(add-hook 'web-mode-before-auto-complete-hooks
          '(lambda ()
             (let ((web-mode-cur-language
  	                (web-mode-language-at-pos)))
               ;; (if (string= web-mode-cur-language "php")
    	         ;;     (yas-activate-extra-mode 'php-mode)
      	       ;;   (yas-deactivate-extra-mode 'php-mode))
               (if (string= web-mode-cur-language "css")
    	             (setq emmet-use-css-transform t)
      	         (setq emmet-use-css-transform nil)))))

;; ;;;http://lgfang.github.io/emacs/emacs-xml.html
;; ;;; Replace xml mode with nxml mode
;; (add-to-list 'auto-mode-alist
;;              (cons (concat "\\." (regexp-opt
;;                                   '("xml" "xsd" "sch"
;;                                     "rng" "xslt" "svg" "rss") t)
;;                            "\\'") 'nxml-mode))
;; (when (> emacs-major-version 21)
;;   (setq magic-mode-alist
;;         (cons '("<\\?xml " . nxml-mode) magic-mode-alist)))
;; (fset 'xml-mode 'nxml-mode)
;; (fset 'html-mode 'nxml-mode)
;; (require 'rng-loc nil t)

;; ;;; Setup hide-show
;; (add-to-list 'hs-special-modes-alist
;;              '(nxml-mode
;;                "<!--\\|<[^/>]*[^/]>" ;; regexp for start block
;;                "-->\\|</[^/>]*[^/]>" ;; regexp for end block
;;                "<!--"
;;                nxml-forward-element
;;                nil))

;; ;;; Where am I
;; (defun nxml-where ()
;;   "Display the hierarchy of XML elements the point is on as a
;; path. from http://www.emacswiki.org/emacs/NxmlMode"
;;   (interactive)
;;   (let ((path nil))
;;     (save-excursion
;;       (save-restriction
;;         (widen)
;;         (while
;;             (and (< (point-min) (point)) ;; Doesn't error if point is at
;;                                          ;; beginning of buffer
;;                  (condition-case nil
;;                      (progn
;;                        (nxml-backward-up-element) ; always returns nil
;;                        t)
;;                    (error nil)))
;;           (setq path (cons (xmltok-start-tag-local-name) path)))
;;         (if (called-interactively-p t)
;;             (message "/%s" (mapconcat 'identity path "/"))
;;           (format "/%s" (mapconcat 'identity path "/")))))))
;; ;;; Other
;; (defun my-nxml-mode-hook ()
;;   (local-set-key (kbd "RET") 'newline-and-indent)
;;   (linum-mode 1)
;;   (hs-minor-mode 1))
;; (add-hook 'nxml-mode-hook 'my-nxml-mode-hook)

;; ;;Highlight the current SGML tag context
;; (require 'hl-tags-mode)
;; (add-hook 'sgml-mode-hook (lambda () (hl-tags-mode 1)))
;; (add-hook 'nxml-mode-hook (lambda () (hl-tags-mode 1)))

;;; emacs-rc-nxml.el ends here
