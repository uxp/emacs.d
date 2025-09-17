;;; init-corfu.el --- Interactive completion in buffers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind
  (("M-/" . dabbrev-completion)
   ("C-M-/" . dabbrev-expand))

  ;; Other useful Dabbrev configuration
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'"))
  (setq dabbrev-case-fold-search nil))

;; Corfu is responsible for interactive completion
(use-package corfu
  :hook ((prog-mode . corfu-mode)
         (org-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))
  :init
  (add-hook 'eglot-managed #'corfu-mode)
  ;;(global-corfu-mode)

  :config
  (defun hplogsdon/corfu-enable-in-minibuffer ()
	(when (where-is-internal #'completion-at-point (list (current-local-map)))
	  ;; Enable/disable auto-completion
	  ;; (setq-local corfu-auto nil)
	  (setq-local corfu-echo-delay nil
				  corfu-popup-info-delay nil)
	  (corfu-mode 1)))

  (dolist (c '(minibuffer-setup-hook eshell-mode-hook))
	(add-hook c #'hplogsdon/corfu-enable-in-minibuffer))

  (dolist (c (list (cons "SPC" " ")
				   (cons "."   ".")
				   (cons "C-(" "\\(")
				   (cons "C-)" "\\)")
				   (cons "C-[" "\\[")
				   (cons "C-]" "\\]")
				   (cons "="   "=")
				   (cons ":"   ":")
				   (cons "C-{" "\\[")
				   (cons "C-}" "\\]")
				   (cons ","   ",")
				   (cons "-"   "-")
				   (cons ")"   ")")
				   (cons "}"   "}")
				   (cons "]"   "]")))
	(define-key corfu-map (kbd (car c)) `(lambda ()
										   (interactive)
										   #'(corfu-quiet)
										   (insert ,(cdr c)))))

  (map corfu-map
	   "C-SPC" #'corfu-insert-separator
	   "C-h"   #'corfu--popup-show
	   "C-t"   #'corfu-insert
	   "C-n"   #'corfu-next
	   "C-p"   #'corfu-previous
	   "C-l"   #'corfu-complete
	   "C-v"   #'corfu-scroll-down
	   "C-S-v" #'corfu-scroll-up
	   "<tab>" #'corfu-complete
	   "<return>" #'(lambda ()
					  (interactive)
					  (corfu-complete)
					  (call-interactively #'newline))
	   "ESC" (defun corfu-quit-minibuffer ()
			   "`escape-quit-minibuffer' but corfu if minibuffer active"
			   (interactive)
			   (when (and (boundp 'corfu--frame)
						  (frame-live-p corfu--frame))
				 (corfu-quit))
			   (keyboard-quit)))
  
  :custom
  (corfu-auto nil)					; Popup appears automatically
  (corfu-auto-delay 0.15)				; 
  (corfu-auto-prefix 3)
  (corfu-quit-no-match 'separator)
  (corfu-preview-current t)				; Show candidate on pointer
  ;; disable corfu in modes where it's disruptive
  (corfu-mode-modes '(not eshell-mode shell-mode term-mode)))

;; Adds more completion source backends for Corfu
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

;; Adjust some of the emacs defaults if we load this module
(use-package emacs
  :ensure nil
  :custom
  ;; limit the height of the *Completions* buffer
  (completions-max-height 15)
  ;; Use TAB for completions first, then indent
  (tab-always-indent 'complete))


(provide 'init-corfu)
;;; init-corfu.el ends here
