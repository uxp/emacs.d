;;; init-whitespace.el --- Special handling for whitespace -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; Show whitespace issues
(use-package whitespace
  :diminish (global-whitespace-mode
              whitespace-mode
              whitespace-newline-mode)
  :bind
  ("C-c T w" . whitespace-mode)
  :hook
  (prog-mode . whitespace-mode)

  :config
  (setq whitespace-line-column 120
        whitespace-style '(face
                           tabs
                           indent
                           tab-mark
                           empty
                           trailing
                           lines-tail)))

(use-package whitespace-cleanup-mode
  :ensure t
  :diminish whitespace-cleanup-mode

  :bind (("C-c T W" . whitespace-cleanup-mode)
         ("C-c e w" . whitespace-cleanup))

  :hook
  (prog-mode . whitespace-cleanup-mode)

  :config
  (global-whitespace-cleanup-mode 1))

(global-set-key [remap just-one-space] 'cycle-spacing)


(provide 'init-whitespace)
;;; init-whitespace.el ends here
