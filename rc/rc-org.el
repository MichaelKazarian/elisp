;;; emacs-rc-org.el --- 

;; Copyright (C) Michael Kazarian
;;
;; Author: Michael Kazarian <michael.kazarian@gmail.com>
;; Keywords: 
;; Requirements: 
;; Status: not intended to be distributed yet

(setq org-tag-alist '((:startgroup . nil)
                      ;; ("@work" . ?w) ("@home" . ?h)
                      ;; ("@tennisclub" . ?t)
                      (:endgroup . nil)
                      ("Україна" . ?u)
                      ("Історія_города_глупова" . ?g)
                      ("Міжнародна_історія_города_глупова" . ?m)
                      ("Цікаве_людознавство" . ?p)
                      ("Компутерщина" . nil)
                      ))

(defun my-org-mode-hook ()
  ;; (visual-line-mode -1)
  ;; (auto-fill-mode)
  (toggle-truncate-lines -1)
  (font-lock-mode)
  (display-line-numbers-mode)
  (hl-line-mode 1)
  ;; (setq org-hide-emphasis-markers t) ;Hiding markup elements in org-mode
  )

(add-hook 'org-mode-hook 'my-org-mode-hook)

;;(load-library "lesson-mode")
;;; emacs-rc-org.el ends here
