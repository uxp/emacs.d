;;; init-flymake.el --- Configure flymake global behavior -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
              ("C-c l" . flymake-show-buffer-diagnostics)
              ("C-c n" . flymake-goto-next-error)
              ("C-c p" . flymake-goto-prev-error)
              ("C-c c" . flymake-start)))

(provide 'init-flymake)
;;; init-flymake.el ends here
