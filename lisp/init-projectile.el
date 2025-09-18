;;; --- Projectile Project configuration

;; Setup some defaults for the builtin ibuffer
(use-package ibuffer
  :ensure nil
  :bind (("C-x C-b" . ibuffer))
  :custom
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-saved-filter-groups
   '(("default"
      ("org" (or (mode . org-mode) (name . "^\\*Org Src")))
      ("emacs" (or (name . "^\\*scratch\\*$") (name . "^\\*Messages\\*$")))
      ("dired" (mode . dired-mode))
      ("help" (or (name . "^\\*Help\\*$") (name . "^\\*helpful"))))))
  :hook
  ((ibuffer-mode . (lambda ()
		     (ibuffer-switch-to-saved-filter-groups "default")))))

(use-package projectile
  :ensure projectile
  :diminish projectile-mode
  
  :bind
  (("C-c p" . projectile-switch-project))

  :init
  (setq-default
   projectile-cache-file (expand-file-name ".projectile-cache" user-emacs-directory)
   projectile-keymap-prefix (kbd "C-c C-p")
   projectile-known-projects-file (expand-file-name ".projectile-bookmarks" user-emacs-directories))

  :config
  (projectile-global-mode 1)
  (setq projectile-enable-caching t
	projectile-create-missing-test-files t
	;;projectile-completion-system 'ivy
	projectile-mode-line-prefix " Proj"
	projectile-mode-line '(:eval (projectile-project-name))
	projectile-use-git-grep t
	projectile-commander-methods nil))


(use-package ibuffer-projectile
  :after (projectile)
  :bind
  (:map ibuffer-mode-map
	("c" . clean-buffer-list)
	("n" . ibuffer-forward-filter-group)
	("p" . ibuffer-backwards-filter-group))
  :hook
  ((ibuffer . (lambda ()
		(ibuffer-projectile-set-filter-groups)
		(unless (eq ibuffer-sorting-mode 'alphabetic)
		  (ibuffer-do-sort-by-alphabetic)))))
