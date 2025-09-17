;;; init-git.el --- Git SCM Support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package git-modes
  :mode
  ("\\'\\.(docker\\|fd\\|rg\\|ag\\|ag\\|git\\)?ignore\\'" . gitignore-mode))


(use-package git-timemachine
  :bind
  (("C-x v t" . git-timemachine-toggle))
  
  :hook
  (git-timemachine-mode . display-line-numbers-mode))


(use-package magit
  :bind (("C-c g" . magit-status)
		 ("C-x g" . magit-status)
		 ("C-x M-g" . magit-dispatch)
		 ("C-M-<up>" . magit-section-up)
		 ("C-M-<down>" . magit-section-down)
		 :map magit-status-mode-map
		 ("TAB" . magit-selection-toggle)
		 ("<C-tab>" . magit-section-cycle)
		 :map magit-branch-section-map
		 ("RET" . magit-checkout))

  :hook
  ((after-save . 'magit-after-save-refresh-status))

  :config
  (setq ;;magit-use-overlays nil
		;;magit-section-visibility-indicator nil
		;;magit-completing-read-function 'ivy-completing-read
		;;magit-push-always-verify nil
		magit-repository-directories '("~/src/")))
