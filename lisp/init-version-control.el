;;; init-version-control.el --- Version control support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; program-specific version control packages are configured separately.
;; see `init-git.el', for example

(use-package 'diff-hl
  :ensure t
  :after (dired)
  :hook ((prog-mode . 'diff-hl-mode)
		 (dired-mode . 'diff-hl-dired-mode)
		 (after-init . 'global-diff-hl-mode))
  :init
  (defconst hplogsdon/diff-hl-mode-hooks '(emacs-lisp-mode-hook
										   conf-space-mode-hook ; .tmux.conf
										   markdown-mode-hook
										   css-mode-hook
										   web-mode-hook
										   sh-mode-hook
										   python-mode-hook
										   yaml-mode-hook ; tmuxp yaml configs
										   c-mode-hook)
	"List of hook of major modes in which `diff-hl-mode' should be enabled.")
  (dolist (hook hplogsdon/diff-hl-mode-hooks)
	(add-hook hook #'diff-hl-flydiff-mode))
  
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

;; TODO: diff-hl-hydra ?
;; (use-package diff-hl-hydra
;;   :after (hydra))

(use-package vc
  :bind (("C-x v =" . hplogsdon/vc-diff)
		 ("C-x v H" . vc-region-history)) ;; new command in emacs 25.x
  :config
  (defun hplogsdon/vc-diff (no-whitespace)
	"Call `vc-diff' as usual if buffer is not modified.
  If the buffer is modified (yet to be saved, dirty) call
  `diff-buffer-with-file'. If NO-WHITESPACE is non-nill, ignore
  all whitespace when doing diff."
	(interactive "P")
	(let* ((no-ws-switch '("-w"))
		   (vc-git-diff-switches (if no-whitespace
									 no-ws-switch
								   vc-git-diff-switches))
		   (vc-diff-switches (if no-whitespace
								 no-ws-switch
							   vc-diff-switches))
		   (diff-switches (if no-whitespace
							  no-ws-switch
							vc-diff-switches))
		   ;; set `current-prefix-arg' no nil so that the HISTORIC arg of
		   ;; `vc-diff' stays nil.
		   current-prefix-arg)
	  (if (buffer-modified-p)
		  (diff-buffer-with-file (current-buffer))
		(call-interactively #'vc-diff))))
  

(provide 'init-version-control)
;;; init-version-control.el ends here
