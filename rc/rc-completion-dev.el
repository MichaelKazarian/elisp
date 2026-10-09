;; -*- lexical-binding: t; -*-
;; Профіль: dev. Доповнення через company (у base не потрібне).
(setq company-idle-delay nil          ; лише вручну
      company-quickhelp-delay 3)

(global-set-key (kbd "\e\em") #'company-complete)

(with-eval-after-load 'company
  (when (fboundp 'company-quickhelp-mode)
    (company-quickhelp-mode 1)))

(add-hook 'after-init-hook #'global-company-mode)
