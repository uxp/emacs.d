;;; init-uniquify.el --- Configure uniquification of buffer name -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'reverse
        uniquify-separator " • "
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re "^\\*"))

(provide 'init-uniquify)
;;; init-uniquify.el ends here
