;; (defun ecb-get-tags-for-non-semantic-files-fixed (orig-fun)
;;   "Get tags for non-semantic files, avoiding CL-RETURN error."
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
;;       ;; Remove the Imenu rescan item.
;;       (when (and tag-list
;;                  (car tag-list)
;;                  (equal (car (car tag-list))
;;                         (car imenu--rescan-item)))
;;         (setq tag-list (cdr tag-list)))
;;       ;; Already grouped? Keep the original hierarchy.
;;       (unless (seq-some
;;                (lambda (tag)
;;                  (or (speedbar-generic-list-positioned-group-p tag)
;;                      (speedbar-generic-list-group-p tag)))
;;                tag-list)
;;         (while methods
;;           (setq tag-list (funcall (car methods) tag-list)
;;                 methods (cdr methods))))
;;       tag-list)))

;; (advice-add 'ecb-get-tags-for-non-semantic-files
;;             :around
;;             #'ecb-get-tags-for-non-semantic-files-fixed)
