;;; init-ibuffer.el --- iBuffer settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package ibuffer
  :straight (:type builtin)
  :bind (("C-x C-b" . ibuffer))
  :commands (ibuffer-current-buffer
             ibuffer-find-file
             ibuffer-do-sort-by-alphabetic)
  :preface
  (defvar protected-buffer '("*scratch*" "*Messages*")
	"Buffers that cannot be killed")
  (defun hplogsdon/protected-buffers ()
	"Protect some buffers from being killed."
	(dolist (buffer protected-buffers)
	  (with-current-buffer buffer
		(emacs-lock-mode 'kill))))
  :init
  (setq ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold)))
  (setq ibuffer-formats '((mark modified read-only locked
                                " " (icon 2 2 :left :elide) (name 18 18 :left :elide)
                                " " (size 9 -1 :right)
                                " " (mode 16 16 :left :elide)
                                " " filename-and-process)
                          (mark modified read-only vc-status-mini
                                " " (name 22 22 :left :elide)
                                " " (size-h 9 -1 :right)
                                " " (mode 14 14 :left :elide)
                                " " (vc-status 12 12 :left)
                                " " vc-relative-file)
                          (mark " " (name 16 -1) " " filename)))
  (setq ibuffer-saved-filter-groups '(("default"
                                       ("org" (or (mode .org-mode) (name . "^\\Org Mode")))
                                       ("emacs" (or (name . "^\\*scratch\\*$") (name . "\\*Messages\\*$")))
                                       ("dired" (mode . dired-mode))
                                       ("terminal" (name . "^\\*Help\\*$")))))
  (hplogsdon/protected-buffers)
  :config
  (add-hook 'ibuffer-mode-hook
             (lambda ()
               (ibuffer-switch-to-saved-filter-groups "default")
               (ibuffer-update nil t)
               (ibuffer-auto-mode 1)))


  (setq ibuffer-show-empty-filter-groups nil))
 

(provide 'init-ibuffer)
;;; init-ibuffer.el ends here
