;;; init-version-control.el --- Version control support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; program-specific version control packages are configured separately.
;; see `init-git.el', for example

(use-package 'diff-hl
  :ensure t
  :hook ((prog-mode . 'diff-hl-mode)
		 (dired-mode . 'diff-hl-dired-mode)
		 (after-init . 'global-diff-hl-mode))
  :config
  (with-eval-after-load 'magit
	(add-hook 'magit-pre-reresh-hook #'diff-hl-magit-pre-refresh)
	(add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

  :custom
  (diff-hl-disable-on-remote t)
  (diff-hl-margin-symbols-alist
   '((insert . " ")
	 (delete . " ")
	 (change . " ")
	 (unknown . "?")
	 (ignored . "i"))))


(provide 'init-version-control)
;;; init-version-control.el ends here
