;;; init-paredit.el --- Paredit mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :init
  (progn
	(defun hplogsdon/maybe-map-paredit-newline ()
	  (unless (or (derived-mode-p 'inferior-emacs-lisp-mode 'cider-repl-mode)
				  (minibufferp))
		(local-set-key (kbd "RET") 'paredit-newline)))
	(add-hook 'paredit-mode-hook 'hplogsdon/maybe-map-pardit-newline))
  
  :config
  (progn
	(defvar paredit-minibuffer-commands '(eval-expression
										  pp-eval-expression
										  eval-expression-with-eldoc
										  ibuffer-do-eval
										  ibuffer-do-view-and-eval)
	  "Interactive commands where paredit should be enabled in minibuffer.")
	(defun hplogsdon/conditionally-enable-paredit-mode ()
	  "Enable paredit during lisp-related minibuffer commands."
	  (when (memq this-command paredit-minibuffer-commands)
		(enable-paredit-mode)))
	;; Use paredit in the minibuffer
	;; https://emacsredux/blog/2013/04/18/evaluate-emacs-lisp-in-the-minibuffer/
	(add-hook 'minibuffer-setup-hook 'hplogsdon/conditionally-enable-paredit-mode))

  :bind (:map paredit-mode-map
		 ([remap kill-sentence] . paredit-kill)
		 ([remap backward-kill-sentence] . nil))

  :hook ((lisp-mode             . enable-paredit-mode)
		 (emacs-lisp-mode       . enable-paredit-mode)
		 (clojure-mode          . enable-paredit-mode)
		 (cider-repl-mode       . enable-paredit-mode)
		 (lisp-interaction-mode . enable-paredit-mode)
		 (ielm-mode             . enable-paredit-mode))

  ;; Paredit Everywhere
  ;; (use-package paredit-everywhere
  ;;   :ensure t
  ;;   :hook ((prog-mode . paredit-everywhere-mode)))


(provide 'init-paredit)
;;; init-paredit.el ends here
