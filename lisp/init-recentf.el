;;; init-recentf.el --- Settings for tracking recent files -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package recentf
  :defer t
  :init (recentf-mode)
  :config
  (setq recentf-max-saved-items 200
        recentf-auto-cleanup 300
        recentf-exclude (list "\\.git/.*\\'"  ; Git contents
                              "/elpa/.*\\'"   ; Package files
                              ".*\\.gz\\'"
                              "TAGS"
                              (concat package-user-dir "/.*-autoloads\\.el\\'")
                              "ido.last")))


(provide 'init-recentf)
;;; init-recentf.el ends here
