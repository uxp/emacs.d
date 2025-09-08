;; init.el --- Howard Logsdon's Emacs Config -*- lexical-binding: t -*-

;; Copyright © Howard Logsdon
;; This config targets Emacs 30.2

(defun reload-user-init-file ()
  (interactive)
  (load-file user-init-file))
(global-set-key (kbd "<f5>") 'reload-user-init-file)
(global-set-key (kbd "C-c C-l") 'reload-user-init-file)


(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "Emacs ready in %s with %d garbage collections"
		     (format "%.2f seconds"
			     (float-time
			      (time-subtract after-init-time before-init-time)))
		     gcs-done)))

(defun is-macos ()
  (string-equal system-type "darwin"))

(require 'use-package)
(setq package-archives '(("elpa"         . "https://elpa.gnu.org/packages/")
			 ("melpa"        . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("org"          . "https://orgmode.org/elpa/")
			 ("gnu"          . "https://elpa.gnu.org/packages/")
			 ("nongnu"       . "https://elpa.nongnu.org/nongnu/"))
      package-archive-priorities '(("elpa" . 30)
                                   ("nongnu" . 25)
				   ("org" . 20)
				   ("nongnu" . 18)
                                   ("melpa-stable" . 15)
                                   ("melpa" . 10)))
;(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
	use-package-expand-minimally t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-pinned-packages
   '(
     (bind-key . "melpa-stable")
     (cider . "melpa-stable")
     (cider-eval-sexp-fu . "melpa-stable")
     (clojure-mode . "melpa-stable")
     (clj-refactor . "melpa-stable")
     (company . "melpa-stable")
     (consult . "elpa")                       ; https://github.com/emacs-straight/consult
     (embark . "elpa")                        ; https://elpa.gnu.org/packages//embark.html
     (embark-consult . "elpa")                ; https://elpa.gnu.org/packages//embark-consult.html
     (exec-path-from-shell . "nongnu")        ; https://github.com/purcell/exec-path-from-shell/blob/master/README.md
     (magit . "melpa-stable")                 ; https://magit.vc/
     (magit-popup . "melpa-stable")
     (markdown-mode . "melpa-stable")         ; https://jblevins.org/projects/markdown-mode/
     (marginalia . "elpa" )                   ; https://www.emacswiki.org/emacs/Marginalia
     (move-text . "melpa-stable")             ; https://github.com/emacsfodder/move-text
     (no-littering . "melpa-stable")          ; https://github.com/emacscollective/no-littering
     (orderless . "melpa-stable")             ; https://github.com/oantolin/orderless
     (org . "org")                            ; https://orgmode.org/
     (org-plus-config . "org")
     (paredit . "melpa-stable")
     (projectile . "melpa-stable")
     (rainbow-delimiters . "melpa-stable")
     (timu-macos-theme . "melpa-stable")
     (use-package . "melpa")
     (vertico . "melpa-stable")               ; https://github.com/minad/vertico
     (visual-regexp . "melpa")                ; https://github.com/emacsmirror/visual-regexp
     (visual-regexp-steroids . "melpa")       ; https://github.com/benma/visual-regexp-steroids.el
     (vundo . "melpa" )                       ; https://github.com/emacs-straight/vundo
     )))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package org
  :pin org
  :ensure nil
  :init
  (setq org-directory "~/org"))

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-result f)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))
  
(use-package bind-key)

(when (is-macos)
  (setq
   mac-right-command-modifier 'super
   mac-command-modifier 'super
   mac-option-modifier 'meta
   mac-left-option-modifier 'meta
   mac-right-option-modifier 'meta
   mac-right-option-modifier 'nil))

(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)

(use-package exec-path-from-shell
  :demand

  :init
  (exec-path-from-shell-initialize))

(use-package no-littering
  :demand

  :config
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory))
  (setq auto-save-file-name-transforms
	`((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(use-package timu-macos-theme
  :ensure t
  :config
  (load-theme 'timu-macos t))

(use-package orderless
  :custom
  (completion-styles '(substring orderless basic)))

(use-package vertico
  :custom
  ;(vertico-scroll-margin 0)  ;; Different scroll margin
  ;(vertico-count 20)         ;; Show more candidates
  (vertico-resize t)         ;; Grow and shrink the vertico minibuffer
  ;(vertico-cycle t)          ;; Enable cyclking for `vertico-next/previous`
  :init
  (vertico-mode 1))

(use-package marginalia
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))

  :init
  (marginalia-mode 1))

(use-package embark
  :ensure t
  :bind (("C-." . embark-act)          ;; Pick some comfortable binding
         ("C-;" . embark-dwim)         ;; Good alternative "M-."
         ("C-h B" . embark-bindings))  ;; alternative for `describe-bindings`

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; show the Embark target at point via Eldoc. You may adjust the Eldoc strategy,
  ;; if you want to see the documentation from multiple providers. Beare that this
  ;; can be a little jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down.
  ;(add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;(setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package consult
  :custom
  (consult-find-command "fd --color=never --full-path ARG OPTS")
  (completion-in-region-function
   (lambda (&rest args)
     (apply (if vertico-mode
		#'consult-completion-in-region
	      #'completion--in-region)
	    args)))

  :config
  (consult-customize consult-line
		     consult-outline
		     consult-imenu
		     consult-mark
		     :preview-key 'any
		     consult-buffer
		     consult-buffer-other-window
		     :preview-key nil)

  :bind (([remap imenu] . consult-imesnu)
	 ([remap yank-pop] . consult-yank-pop)
	 ([remap repeat-complex-command] . consult-complex-command)
	 ("s-f" . consult-line)))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package paredit
  :ensure t
  :hook ((clojure-mode . paredit-mode)))

(use-package projectile
  :init
  (setq projetile-create-missing-test-files t)
  (setq projectile-project-search-path '("~/src"))
  :config
  (projectile-global-mode))

(use-package magit)

(use-package markdown-mode
  :config
  (defun markdown-display-inline-images ()
    "Add inline image overlays to image links in the buffer.
    This can be toggled with `markdown-toggle-inline-images`
    or \\[markdown-toggle-inline-images]."
    (interactive)
    (unless (display-images-p)
      (error "Cannot show images"))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (re-search-forward markdown-regex-link-inline nil t)
               (let* ((start match-beginning 0))
                 (imagep (match-beginning 1))
                 (end (match-end 0))
                 (file (funcall markdown-translate-filename-function (match-string-no-properties 6))))
               (when (and imagep
                          (not (zerop (length file)))))
               (let* ((download-file (funcall markdown-translate-filename-function file))
                      (valid-url (ignore-errors
                                   (member (downcase (url-type (url-generic-parse-url download-file)))
                                           markdown-remote-image-protocols))))
                 (if (and markdown-display-remote-images valid-url)
                   (setq file (markdown--get-remote-iamge download-file))
                   (when (not valid-url)
                     ;; strip query params
                     (setq file (replace-regexp-in-string "?.+\\'" "" file))
                     (unless (file-exists-p file)
                       (setq file (url-unhex-string file)))))))
        (when (file-exists-p file)
          (let* ((abspath (if (file-name-absolute-p file)
                            file
                            (concat default-directory file)))
                 (image
                   (cond ((and markdown-max-image-size
                               (image-type-available-p 'imagemagik))
                          (create-image
                            abspath 'imagemagik nil
                            :max-width (car markdown-max-image-size)
                            :max-height (cdr markdown-max-image-size)))
                         (markdown-max-image-size
                           (create-image abspath nil nil
                                         :max-width (car markdown-max-image-size)
                                         :max-height (cdr markdown-max-image-size)))
                         (t (create-image abspath)))))
            (when image
              (let ((ov (make-overlay start end)))
                (overlay-put ov 'display image)
                (overlay-put ov 'face 'default)
                (push ov markdown-inline-image-overlays)))))))))

(use-package yaml-mode)
(use-package hcl-mode)
(use-package terraform-mode)
(use-package dockerfile-mode)

(use-package clj-refactor
  :after (cider)
  :diminish clj-refactor-mode
  :config
  (setq cljr-cljc-clojure-test-declaration "[clojure.test :refer [deftest testing is are use-fixtures run-tests join-fixtures]]"
        cljr-cljc-clojure-test-declaration "[clojure.test :refer [deftest testing is are use-fixtures run-tests join-fixtures]]"
        cljr-clojure-test-declaration "[clojure.test :refer [deftest testing is are use-fixtures run-tests join-fixtures]]"
        cljr-eagerly-build-asts-on-startup nil
        cljr-warn-on-eval nil)
  :hook ((clojurex-mode-hook
          clojurescript-mode-hook
          clojurec-mode-hook
          clojure-mode-hook) . clj-refactor-mode))

(with-eval-after-load 'clojure-mode
  (with-current-buffer (get-buffer-create "*scratch-clj*")
                       (clojure-mode))

  (with-current-buffer (get-buffer-create "*scratch*")
                       (lisp-interaction-mode)))

(with-eval-after-load 'projectile
  (setq projectile-project-root-files-bottom-up
        (cons "deps.edn" projectile-project-root-files-bottom-up)))

;; should be nil/turned off so we can always see whats happening
(setq eval-expression-print-level nil)

(use-package rainbow-delimiters
  :hook
  ((emacs-lisp-mode . rainbow-delimiters-mode)
   (ielm-mode . rainbow-delimiters-mode)
   (lisp-interaction-mode . rainbow-delimiters-mode)
   (lisp-mode . rainbow-delimiters-mode)))

(use-package cider
  :after '(evil)
  :ensure t
  :init
  (setq cider-dynamic-indentation nil
	cider-font-lock-dynamically nil
	cider-font-lock-reader-conditionals nil)
  (setq cider-clojure-cli-global-options "")
  :config
  (evil-define-key '(normal visual) cider-repl-mode-map
    (kbd "SPC,") 'evil-switch-to-windows-last-buffer))

(use-package time
  :custom
  (world-clock-time-format "%Z, %d %b, %I:%M %p")
  (world-clock-list
   '(("America/Denver" "Salt Lake")
     ("America/Chicago" "Chicago")))

  :init
  (add-to-list 'display-buffer-alist
	       '("\\*wclock.*"
		 (display-buffer-below-selected)
		 (window-height . fit-window-to-buffer)
		 (window-parameters .((select . t))))
	       t))

(use-package vundo)

(provide 'init)
