;;; init-eglot.el --- LSP Support via eglot -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package eglot
  :after (:all xref)
  :hook ((clojure-mode . eglot-ensure))
  :bind (:map (eglot-mode-map
               ("C-<down-mouse-1>" . #'xref-find-definitions)
               ("C-S-<down-mouse-1>" . #'xref-find-references)
               ("C-c a r" . #'eglot-rename)
               ("C-c C-c" . #'eglot-code-actions)))
  :custom
  (eglot-configm-server-initiated-edits nil)
  (eglot-autoshutdown t)
  (eglot-send-changes-idle-time 0.1)
  :config
  (defun hplogsdon/gfm-unescape-string (string)
    "Remove backslash-escape of punctuation characters in STRING"
    (replace-regexp-in-string  "[\\\\]\\([][!\"#$%&'()*+,./:;<=>?@\\^_`{|}~-]\\)" "\\1" string))

  (advice-add 'eglot--format-markup :filter-return 'hplogsdon/gfm-unescape-string)

  (defun hplogsdon/add-eglot-to-prog-menu (old startmenu click)
    "Add useful Eglot functions to the prog-mode context menu."
    (let ((menu (funcall old startmenu click))
          (identifier (save-excursion
                        (mouse-set-point click)
                        (xref-backend-identifier-at-point
                         (xref-find-backend)))))
      (when identifier
        (define-key-after menu [eglot-find-impl]
          `(menu-item "Find Implementations" eglot-find-implementation
                      :help ,(format "Find implementations of `%s`" identifier))
          'xref-find-ref))
      menu))

  (advice-add 'prog-context-menu :around #'hplogsdon/add-eglot-to-prog-menu)
)


(provide 'init-eglot)
;;; init-eglot.el ends here
