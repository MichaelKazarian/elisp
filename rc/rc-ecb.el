;;; emacs-rc-ecb.el --- Emacs Code Browser settings -*- lexical-binding: t; -*-

;; Copyright (C) 2009 Michael Kazarian
;; Author: michael.kazarian@gmail.com

;(require 'ecb)
(require 'ecb-autoloads)

(defun ecb-display-buffer-at-bottom-around (orig-fun &rest args)
"Work around ECB window handling issues (originally in Emacs 27).
Use display-buffer-use-some-window' instead of
display-buffer-at-bottom' when displaying buffers in the ECB frame."
  (if (and (bound-and-true-p ecb-minor-mode)
           (bound-and-true-p ecb-frame)
           (equal (selected-frame) ecb-frame))
      (apply #'display-buffer-use-some-window args)
    (apply orig-fun args)))

(advice-add 'display-buffer-at-bottom
            :around #'ecb-display-buffer-at-bottom-around)

;; Diagnostics.
(setq stack-trace-on-error t)

;; Key bindings.
(global-set-key (kbd "\e\el")  #'ecb-toggle-ecb-windows)
(global-set-key (kbd "\e\eea") #'ecb-activate)
(global-set-key (kbd "\e\eed") #'ecb-deactivate)

;; General settings.
(setq ecb-auto-activate nil
      ecb-auto-compatibility-check nil
      ecb-cache-directory-contents '((".*" . 50))
      ecb-directories-menu-user-extension-function nil
      ecb-gzip-setup 'cons
      ecb-history-menu-user-extension-function nil
      ecb-primary-secondary-mouse-buttons 'mouse-1--C-mouse-1
      ecb-layout-name "left6"
      ecb-methods-menu-user-extension-function nil
      ecb-prescan-directories-for-emptyness nil
      ecb-sources-menu-user-extension-function nil
      ecb-tar-setup 'cons
      ecb-tip-of-the-day nil
      ecb-version-check nil
      ecb-windows-height 0.2
      ecb-windows-width 0.2)

;; Compilation window.
(setq ecb-compile-window-height 12
      ecb-compile-window-temporally-enlarge 'after-selection
      ecb-compile-window-width 'edit-window)

;; Buffers displayed in ECB's compilation window.
(setq ecb-compilation-buffer-names
      '(("*shell*" . nil)
        ("\\*[cC]ompilation.*\\*" . t)
        ("\\*i?grep.*\\*" . t)
        ("*Completions*" . nil)
        ("*Backtrace*" . nil)
        ("*Compile-log*" . nil)
        ("*Messages*" . nil)))

;; --- ECB non semantic ---
(defun my-ecb-expand-non-semantic-methods (&rest _)
  "Expand all nodes in ECB Methods for non-semantic files."
  (when (and (featurep 'ecb)
             (get-buffer ecb-methods-buffer-name)
             (not (ecb-methods-get-data-store
                   'semantic-symbol->name-assoc-list)))
    (ecb-expand-methods-node-internal
     (with-current-buffer ecb-methods-buffer-name
       (tree-buffer-get-root))
     1 t nil t)))

(defun my-ecb-update-methods-buffer (&rest _)
  "Expand non-semantic Methods after updating."
  (run-at-time 0 nil #'my-ecb-expand-non-semantic-methods))

(with-eval-after-load 'ecb-method-browser
  (advice-add 'ecb-update-methods-buffer--internal
              :after #'my-ecb-update-methods-buffer))

;; (advice-add 'semantic-active-p :override (lambda (&rest _args) nil))

(setq ecb-process-non-semantic-files t)
(setq speedbar-use-imenu-flag t)

(setq ecb-non-semantic-parsing-function
      '((python-mode . speedbar-fetch-dynamic-imenu)
        (go-mode   . speedbar-fetch-dynamic-imenu)
        (go-ts-mode     . speedbar-fetch-dynamic-imenu)))

;; Авто-оновлення imenu
(setq imenu-auto-rescan t)

(defun my-ecb-find-tag-at-or-before (node position)
  "Find the nearest preceding Speedbar tag in NODE."
  (let ((best-tag nil)
        (best-position -1))
    (cl-labels
        ((walk (node)
           (let* ((data (tree-node->data node))
                  (markers (and (listp data)
                                (nth 4 data)))
                  (start (and (vectorp markers)
                              (> (length markers) 0)
                              (aref markers 0)))
                  (tag-position
                   (and (markerp start)
                        (marker-position start))))
             (when (and tag-position
                        (<= tag-position position)
                        (> tag-position best-position))
               (setq best-tag data
                     best-position tag-position))
             (dolist (child (tree-node->children node))
               (walk child)))))
      (walk node))
    best-tag))

(defun my-ecb-highlight-imenu-function (&rest _)
  "Highlight the nearest preceding Imenu entry in ECB Methods."
  (when (and (featurep 'ecb)
             (buffer-file-name)
             (boundp 'ecb-methods-buffer-name)
             (get-buffer ecb-methods-buffer-name)
             (not (ecb-methods-get-data-store
                   'semantic-symbol->name-assoc-list)))
    (let ((source-buffer (current-buffer))
          (position (point)))
      (with-current-buffer ecb-methods-buffer-name
        (let* ((root (tree-buffer-get-root))
               (tag (and root
                         (my-ecb-find-tag-at-or-before
                          root
                          position))))
          (if tag
              (tree-buffer-highlight-node-by-data/name tag)
            (tree-buffer-highlight-node-by-data/name nil)))))))

(defun my-speedbar-tag-list-flat-p (tags)
  "Return non-nil if TAGS contains no Speedbar groups."
  (and tags
       (not (seq-some
             (lambda (tag)
               (or (speedbar-generic-list-positioned-group-p tag)
                   (speedbar-generic-list-group-p tag)))
             tags))))

(defun my-ecb-get-tags-for-non-semantic-files (orig-fun &rest args)
  "Preserve existing groups and wrap flat tag lists in a root group."
  (let ((speedbar-tag-hierarchy-method nil)
        (tag-list (apply orig-fun args)))
    (when (my-speedbar-tag-list-flat-p tag-list)
      (setq tag-list
            (list (cons "root" tag-list))))
    tag-list))

(defvar my-ecb-semantic-active-p-count 0)

(with-eval-after-load 'ecb-method-browser
  (advice-add 'ecb-tag-sync
              :after
              #'my-ecb-highlight-imenu-function)
  (advice-add 'ecb-get-tags-for-non-semantic-files
              :around
              #'my-ecb-get-tags-for-non-semantic-files))


;; variant with nodetype
;; (defun my-ecb-highlight-imenu-function (&rest _)
;;   "Highlight the nearest preceding Imenu entry in ECB Methods."
;;   (when (and (featurep 'ecb)
;;              (buffer-file-name)
;;              (boundp 'ecb-methods-buffer-name)
;;              (get-buffer ecb-methods-buffer-name)
;;              (not (ecb-methods-get-data-store
;;                    'semantic-symbol->name-assoc-list)))
;;     (let ((source-buffer (current-buffer))
;;           (position (point)))
;;       (with-current-buffer ecb-methods-buffer-name
;;         (let* ((root (tree-buffer-get-root))
;;                (children (and root (tree-node->children root)))
;;                ;; Go: functions are grouped under "func".
;;                ;; Other modes: use "Functions" group if present,
;;                ;; otherwise search the whole tree.
;;                (func-node
;;                 (or
;;                  (seq-find
;;                   (lambda (node)
;;                     (member
;;                      (substring-no-properties
;;                       (tree-node->name node))
;;                      '("func" "Functions")))
;;                   children)
;;                  root))
;;                (tag (and func-node
;;                          (my-ecb-find-tag-at-or-before
;;                           func-node position))))
;;           (if tag
;;               (tree-buffer-highlight-node-by-data/name tag)
;;             (tree-buffer-highlight-node-by-data/name nil)))))))


;; (defun my-ecb-semantic-active-p (&rest _)
;;   (message "ECB: semantic-active-p overridden")
;;   nil)

;; (defun my-ecb-install-advice ()
;;   (message "ECB: installing advice")
;;   (advice-add 'ecb--semantic-active-p
;;               :override #'my-ecb-semantic-active-p))

;; (if (featurep 'ecb-method-browser)
;;     (my-ecb-install-advice)
;;   (with-eval-after-load 'ecb-method-browser
;;     (my-ecb-install-advice)))


;; (advice-add 'ecb--semantic-active-p
;;             :override #'my-ecb-semantic-active-p)
