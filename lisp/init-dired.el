;;; init-dired.el --- Dired customization -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun hplogsdon/dired-open-externally ()
  (interactive)
  (let* ((file-list (dired-get-marked-files))
         (proceed-p (if (<= (length file-list) 5)
                        t
                      (y-or-no-p "Open more than 5 files?"))))
    (when (proceed-p
      (cond
        (*is-windows*
          (mapc (lambda (file-path)
                  (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" file-path t t)))
                file-list))
        (*is-macos*
          (mapc (lambda (file-path)
                  (shell-command (format "open \"%s\"" file-path)))
                file-list))
        (*is-linux*
          (mapc (lambda (file-path)
                  (let (process-connection-type)
                    (start-process "" nil "xdg-open" file-path)))
                file-list)))))))


(defun hplogsdon/dired-mode-defaults ()
  "Configure the dired-mode buffer."
  (dired-omit-mode 1)
  (dired-hide-details-mode)
  (diff-hl-dired-mode))


(use-package dired
  :bind
  <<bindings-from-table(table=dired-bindings-table)>>

  :config
  (progn 
    (require 'dired-x)

    (setq dired-omit-verbose nil)

    (setq dired-auto-revert-buffer t
          dired-listing-switches "-alhF --group-directories-first -v"
          dired-omit-files "^\\.[^.].*$")

    (dolist (fun '(dired-do-rename
                   dired-create-directory
                   wdired-abort-changes))
      (eval `(defadvice ,fun (after revert-buffer activate)
               (revert-buffer))))

    (add-hook 'dired-mode-hook hplogsdon/dired-mode-defaults)))


(provide 'init-dired)
;;; init-dired.el ends here
