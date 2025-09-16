;;; init-lisp.el --- Emacs lisp settings, and common config for other lisps -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun hplogsdon/eval-last-sexp-or-region (prefix)
  "Eval region from BEG to END if active, otherwise the last sexp."
  (interactive "P")
  (if (and (mark) (use-region-p))
	  (eval-region (min (point) (mark)) (max (point) (mark)))
	(pp-eval-last-sexp prefix)))

;; TODO: look into `litable' as an alternative
(defun hplogsdon/elisp-eval-and-comment-output ()
  "Add the output of the sexp as a comment after the sexp"
  (interactive)
  (save-excursion
	(end-of-line)
	(condition-case nil
		(printc (concat " ; -> " (pp-to-string (eval (preceding-sexp))))
				(current-buffer))
	  (error (message "Invalid expression")))))

(defun hplogsdon/elisp-eval-region ()
  (interactive)
  (if (region-active-p)
	  (progn
		(eval-region (region-beginning)
					 (region-end))
		(deactivate-mark))
	(eval-expression)))

(defun hplogsdon/elisp-headerize ()
  "Adds a header and footer to an elisp buffer for Flycheck"
  (interactive)
  (let ((fname (if (buffer-file-name)
				   (file-name-nondirectory (buffer-file-name))
				 (error "This buffer is not visiting a file"))))
	(save-excursion
	  (goto-char (point-min))
	  (insert ";;; " fname " --- Description -*- lexical-binding: t -*-\n"
			  ";;; Commentary:\n"
			  ";;; Code:\n\n")
	  (goto-char (point-max))
	  (insert ";;; " fname " ends here\n"))))

(defun hplogsdon/elisp-register-elc-delete-on-save ()
  "If you're saving an elisp file, the .elc is likely invalid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
			'(lambda ()
			   (when (file-exists-p (concat buffer-file-name "c"))
				 (delete-file (concat buffer-file-name "c"))))))

(use-package lisp-mode
  :defer t
  :hook ((emacs-lisp-mode . outline-minor-mode)
		 (emacs-lisp-mode . reveal-mode))
  :bind (("C-x e" . hplogsdon/elisp-eval-and-comment-output))
  :mode (("\\.el$" . emacs-lisp-mode))
  :init
  (setq initial-major-mode 'emacs-lisp-mode)
  (hplogsdon/elisp-register-elc-delete-on-save)

  :config
  (turn-on-eldoc-mode)
  (hplogsdon/elisp-register-elc-delete-on-save)
  (paredit-mode +1))


(use-package color-identifiers-mode
  :ensure t
  :hook ((emacs-lisp-mode . color-identifiers-mode)))


(provide 'init-lisp)
;;; init-lisp.el ends here
