;;; init-orgmode.el --- Org Mode Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package org
  :pin "orgmode"
  :mode ("\\.org\\'" . org-mode)
  :defer t
  :bind (("C-c a" . org-agenda)
		 ("C-c c" . org-capture)
		 ;("C-c j" . org-journal)
		 (:map org-mode-map
			   (("M-p" . outline-previous-visible-heading)
				("M-n" . outline-next-visible-heading)
				("C-c C-p" . eaf-org-export-to-pdf-and-open)
				("C-c ;" . nil))))

  :custom
  (org-return-follows-link t)
  (org-export-backends (quote (ascii html latex md odt)))
  (org-confirm-babel-evaluate 'nil)
  (org-deadline-warning-days 7)
  (org-agenda-window-setup 'other-window)
  (org-babel-load-languges
   '((emacs-lisp . t)
	 (python . t)
	 (dot . t)))
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
	 (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))

  :custom-face
  (org-agenda-current-time ((t (:forground "spring green"))))

  :config
  (unless (version< org-version "9.2")
	(require 'org-tempo))
  (when (or (file-directory-p "~/org/agenda") (file-directory-p "~/org/journal"))
	(setq org-agenda-files (list "~/org/agenda" "~/org/journal"))))

(use-package org-journal
  :bind (("C-c j n" . org-journal-new-entry)
	   ("C-c j t" . org-journal-today))

  :custom
  (org-journal-date-prefix "#+TITLE: ")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-dir "~/org/journal")
  (org-journal-date-format "%Y-%m-%d")

  :config
  (defun org-journal-today ()
	(interactive)
	(org-journal-new-entry t)))



(provide 'init-orgmode)
;;; init-orgmode.el ends here
