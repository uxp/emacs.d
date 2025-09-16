;;; init-hippy-expand.el --- Settings for hippie-expand -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package hippie-exp
  :ensure nil
  :bind ("M-/" . hippie-expand)
  
  :init
  (defadvice hippie-expand (around hippie-expand-case-fold activate)
    "Try to do case-sensitive matching (not effective with all functions)."
    (let ((case-fold-search nil))
      ad-do-it))
  
  :config
  (setq hippie-expand-try-functions-list
        '(;; Try to expand word "dynamically", searching just the current buffer
          try-expand-dabbrev
          ;; Try to expand word "dynamically", searching ...?
          try-expand-dabbrev-visible
          ;; Try to expand word "dynamically", searching all other buffers.
          try-expand-dabbrev-all-buffers
          ;; Try to expand word "dynamically", searching the kill ring
          try-expand-dabbrev-from-kill
          ;; Try to complete text as a filename, as many characters are unique
          try-complete-file-name-partially
          ;; Try to complete text as a filename.
          try-complete-file-name
          ;; Try to complete before point according to all abbrev tables.
          try-expand-all-abbrevs
          ;; Try to complete the current line to a list in the buffer
          try-expand-list
          ;; Try to complete the current line to a line in the buffer
          try-expand-line
          ;; Try to complete the current line to an entire line in a different buffer.
          try-expand-line-all-buffers
          ;; Try to complete text using flyspell
          ;try-flyspell
          ;; Try to complete as an Emacs Lisp symbol, as many characters are unique.
          try-complete-lisp-symbol-partially
          ;; Try to complete word as an Emacs Lisp symbol.
          try-complete-lisp-symbol)))


(provide 'init-hippy-expand)
;;; init-hippy-expand.el ends here
