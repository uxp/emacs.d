;;; init-mmm.el --- Multiple Major Modes support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package mmm-mode
  :ensure t
  :defer t
  :config
  (setq mmm-global-mode 'buffers-with-submode-classes)
  (setq mmm-submode-mode-line-format "~M > [~m]"
		mmm-primary-mode-display-name t
		mmm-buffer-mode-display-name t)
  (setq mmm-submode-decoration-level 3)

  ;;(mmm-add-mode-ext-classes 'html-mode "\\.php\\'" 'html-php)

  (unless (boundp 'editing-prefix)
	(define-prefix-command 'editing-prefix))
  (define-key editing-prefix (kbd "m") 'mmm-mode) ;; enable mmm on region

  ;; Submode classes
  ;;(mmm-add-classes
  ;; '((embedded-css
  ;;	  :submode css
  ;;	  :face mmm-declaration-submode-face
  ;;    :front "<style[^>]&>"
  ;;	  :back "</style>")))

  ;; Submode groups
  ;;(mmm-add-to-group 'html-js '((js-html
  ;;                              :submode javascript
  ;;                              :face mmm-code-submode-face
  ;;                              :front "%=%"
  ;;                              :back "%=%"
  ;;                              :end-not-begin t)))
)
