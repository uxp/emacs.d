;;; init-claude.el --- Example -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package claude-code
  :ensure t
  :after (:any eat vterm)
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)

  :config
  (claude-code-mode)
  ;; eat is the default, but we could change to vterm
  ;;(setq claude-code-terminal-backend 'vterm)
  ;;(setq claude-code-terminal-backend 'eat)

  :bind-keymap
  ("C-c C" . claude-code-command-mode)

(provide 'init-claude)
;;; init-claude.el ends here
