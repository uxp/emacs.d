;;; init-themes.el --- Defaults for Themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package monokai-pro-theme
  :ensure t
  :init
  (load-theme 'monokai-pro t))

(use-package all-the-icons-completion
  :ensure t
  :config
  (require 'all-the-icons)
  (all-the-icons-completion-mode))

(provide 'init-themes)
;;; init-themes.el ends here
