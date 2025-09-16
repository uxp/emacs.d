;;; init-xref.el --- Cross Reference (xref) configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package xref
  :straight t
  :defer t
  :bind (("s-[" . #'xref-go-back)
         ("s-]" . #'xref-go-forward)
         ("C-c r" . #'xref-find-references)
         ("C-c d" . #'xref-find-definitions))
  :config (add-to-list 'xref-prompt-for-identifier #'xref-find-references 'append)
  :custom
  (xref-auto-jump-to-first-xref t))


(provide 'init-xref)
;;; init-xref.el ends here
