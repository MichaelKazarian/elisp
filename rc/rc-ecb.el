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


;; (defun ecb-get-tags-for-non-semantic-files ()
;;   "Get a tag-list for current source-file via Speedbar."
;;   (require 'imenu)
;;   (if (member major-mode ecb-non-semantic-exclude-modes)
;;       nil
;;     (let* ((lst
;;             (let ((speedbar-dynamic-tags-function-list
;;                    (if (not (assoc major-mode
;;                                    ecb-non-semantic-parsing-function))
;;                        (list (cons 'speedbar-fetch-dynamic-imenu 'identity)
;;                              (cons 'speedbar-fetch-dynamic-etags 'identity))
;;                      (list (cons (cdr (assoc major-mode
;;                                              ecb-non-semantic-parsing-function))
;;                                  'identity)))))
;;               (speedbar-fetch-dynamic-tags
;;                (ecb-buffer-file-name (current-buffer)))))
;;            (tag-list (cdr lst))
;;            (methods speedbar-tag-hierarchy-method))

;;       ;; Remove Imenu rescan item.
;;       (when (and tag-list
;;                  (ecb-string= (caar tag-list)
;;                               (car imenu--rescan-item)))
;;         (setq tag-list (cdr tag-list)))

;;       ;; Wrap flat tag lists in a synthetic root group.
;;       (when (my-speedbar-tag-list-flat-p tag-list)
;;         (setq tag-list
;;               (list (cons "root" tag-list))))

;;       ;; Do not regroup existing groups.
;;       (unless (seq-some
;;                (lambda (tag)
;;                  (or (speedbar-generic-list-positioned-group-p tag)
;;                      (speedbar-generic-list-group-p tag)))
;;                tag-list)
;;         (while methods
;;           (setq tag-list (funcall (car methods) tag-list)
;;                 methods (cdr methods))))

;;       tag-list)))

;; (defun my-ecb-tag-position-range (node)
;;   "Return the position range of NODE's Speedbar tag."
;;   (let* ((data (tree-node->data node))
;;          (range (and (listp data) (nth 4 data))))
;;     (when (and (vectorp range)
;;                (= (length range) 2))
;;       (let ((start (aref range 0))
;;             (end   (aref range 1)))
;;         (cons
;;          (cond
;;           ((markerp start) (marker-position start))
;;           ((overlayp start) (overlay-start start))
;;           ((integerp start) start))
;;          (cond
;;           ((markerp end) (marker-position end))
;;           ((overlayp end) (overlay-end end))
;;           ((integerp end) end)))))))

;; (defun my-ecb-collect-tag-nodes (node)
;;   "Recursively collect all leaf tag nodes below NODE."
;;   (let ((children (tree-node->children node)))
;;     (if children
;;         (apply #'append
;;                (mapcar #'my-ecb-collect-tag-nodes children))
;;       (if (tree-node->data node)
;;           (list node)
;;         nil))))

;; (defun my-ecb-find-current-tag (root position)
;;   "Find the most relevant tag in ROOT for POSITION."
;;   (let ((best-containing nil)
;;         (best-containing-size most-positive-fixnum)
;;         (best-previous nil)
;;         (best-previous-position -1))
;;     (dolist (node (my-ecb-collect-tag-nodes root))
;;       (let* ((range (my-ecb-tag-position-range node))
;;              (start (car-safe range))
;;              (end   (cdr-safe range)))
;;         (when (and start end (<= start position))
;;           ;; Prefer the smallest range containing point.
;;           (if (and (<= start position)
;;                    (<= position end)
;;                    (< (- end start) best-containing-size))
;;               (setq best-containing node
;;                     best-containing-size (- end start)))
;;           ;; Otherwise remember the nearest preceding tag.
;;           (when (> start best-previous-position)
;;             (setq best-previous node
;;                   best-previous-position start))))
;;     (or best-containing best-previous))))

;; (defun my-ecb-highlight-imenu-function (&rest _)
;;   "Highlight the current Imenu tag in ECB Methods."
;;   (when (and (boundp 'ecb-methods-buffer-name)
;;              (buffer-live-p (get-buffer ecb-methods-buffer-name)))
;;     (let ((source-buffer (current-buffer))
;;           (position (point)))
;;       (with-current-buffer ecb-methods-buffer-name
;;         (let* ((root (tree-buffer-get-root))
;;                (node (and root
;;                           (my-ecb-find-current-tag root position))))
;;           (tree-buffer-highlight-node-by-data/name
;;            (and node (tree-node->data node))))))))

;; (defun my-speedbar-tag-list-flat-p (tags)
;;   "Return non-nil if TAGS contains no Speedbar groups."
;;   (and tags
;;        (not (seq-some
;;              (lambda (tag)
;;                (or (speedbar-generic-list-positioned-group-p tag)
;;                    (speedbar-generic-list-group-p tag)))
;;              tags))))

;; (with-eval-after-load 'ecb-method-browser
;;   (advice-add 'ecb-tag-sync
;;               :after #'my-ecb-highlight-imenu-function))

(defun my-ecb-find-tag-at-or-before (node position)
  "Find the nearest preceding tag among NODE's children."
  (let ((best-tag nil)
        (best-position -1))
    (dolist (child (tree-node->children node))
      (let* ((tag (tree-node->data child))
             (start (and (semantic-tag-p tag)
                         (semantic-tag-start tag)))
             (tag-position
              (cond
               ((and (markerp start)
                     (marker-buffer start))
                (marker-position start))
               ((integerp start)
                start))))
        (when (and tag-position
                   (<= tag-position position)
                   (> tag-position best-position))
          (setq best-tag tag
                best-position tag-position))))
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
               (children (and root (tree-node->children root)))
               (tag
                (when root
                  (if children
                      (car
                       (sort
                        (delq nil
                              (mapcar
                               (lambda (group)
                                 (my-ecb-find-tag-at-or-before
                                  group position))
                               children))
                        (lambda (a b)
                          (> (marker-position (semantic-tag-start a))
                             (marker-position (semantic-tag-start b))))))
                    (my-ecb-find-tag-at-or-before root position)))))
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

(with-eval-after-load 'ecb-method-browser
  (advice-add 'ecb-tag-sync
              :after
              #'my-ecb-highlight-imenu-function)
  (advice-add 'ecb-get-tags-for-non-semantic-files
              :around
              #'my-ecb-get-tags-for-non-semantic-files))
