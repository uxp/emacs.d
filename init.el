;; init.el --- -*- lexical-binding: t -*-

;; This config targets Emacs 29.4.1
;; Written by hplogsdon (https://gitlab.com/hplogdon/dotfiles)

;; Produce backtraces on error: helpful for startup issues
;;(setq debug-on-error t)

(let ((minver "29.1"))
  (when (version< emacs-version minver)
	(error "Emacs is too old.")))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-benchmarking)


(defun is-macos ()
  (string-equal system-type "darwin"))

(defun reload-user-init-file ()
  (interactive)
  (load-file user-init-file))

(require 'use-package)
(setq package-archives '(("melpa-stable" . "https://stable.melpa.org/packages/")
						 ("melpa"        . "https://melpa.org/packages/")
						 ("orgmode"      . "https://orgmode.org/elpa/")
						 ("nongnu"       . "https://elpa.nongnu.org/nongnu/")
						 ("gnu"          . "https://elpa.gnu.org/packages/")
						 ("tromey"       . "https://tromey.com/elpa/"))
      package-archive-priorities '(("melpa-stable" . 30)
								   ("orgmode"      . 30)
                                   ("nongnu"       . 20)
								   ("gnu"          . 10)
								   ("tromey"       . 10)
                                   ("melpa"        . 0)))

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
 '(package-selected-packages
   '(auto-package-update
	 bind-key
	 cider
	 cider-eval-sexp-fu
	 clojure-mode
	 clj-refactor
	 company
	 consult                                  ; https://github.com/emacs-straight/consult
	 dockerfile-mode
	 eldoc
	 embark                                   ; https://elpa.gnu.org/packages//embark.html
	 embark-consult
	 exec-path-from-shell
	 magit
	 magit-popup
	 marginalia
	 markdown-mode
	 monokai-pro-theme
	 move-text
     no-littering
     orderless
     org
	 org-journal
	 org-plus-config
     ox-jekyll-md
	 paredit
	 projectile
	 pyenv-mode
	 rainbow-delimiters
	 terraform-mode
	 vertico
	 visual-regexp
	 visual-regexp-steroids
	 vundo
	 yaml-mode
	 yasnippet)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package emacs
  :custom
  (menu-bar-mode nil)                            ;; Disable the menu bar
  (scroll-bar-mode nil)                          ;; Disable the scroll bar
  (tool-bar-mode nil)                            ;; Disable the tool bar
  (inhibit-startup-screen t)                     ;; Disable the welcome screen

  (delete-selection-mode t)                      ;; Select text and delete it by typing
  (electric-pair-mode t)                         ;; Turn on auto-paren pairing

  (blink-cursor-mode nil)                        ;; dont blink cursor/point
  (global-auto-revert-mode t)                    ;; Automatically reload file if modified externally
  
  (dired-kill-when-opening-new-dired-buffer t)   ;; Dired don't create new buffer
  (recentf-mode t)                               ;; enable recent file mode

  (global-visual-line-mode t)                    ;; Enable truncated loines
  (display-line-numbers-type 'relative)          ;; Relative line numbers
  (global-display-line-numbers-mode t)           ;; Display line numbers

  ;;(mouse-wheel-progressive-speed nil)            ;; Disable progressive speed when scrolling
  (scroll-conservatively 10)                     ;; Smooth scrolling
  ;;(scroll-margin 8)

  (tab-width 4)                                  ;; Space is cheap. expand tabs to 4

  (make-backup-files nil)                        ;; Stop creating ~ backup files
  (auto-save-default nil)                        ;; Stop creating # autosave files

  :hook
  (prog-mode . (lambda () (hs-minor-mode t)))    ;; Enable Folding hide/show globally

  :config
  ;; Move custom variables to a separate file and load it. Avoids filling up init.el with unneccessary variables
  (setq custom-file (locate-user-emacs-file "custom.el"))
  (load custom-file 'noerror 'nomessage)         ;; load the custom vars (swallow errors)

  :bind (("s-=" . text-scale-increase)           ;; Zoom in/out
	 ("s--" . text-scale-decrease)
	 ("<f5>" . reload-user-init-file)        ;; f5 reloads this file
	 ("C-c C-l" . reload-user-init-file)))


(use-package org
  :pin orgmode
  :ensure nil
  :init
  (setq org-directory "~/org")
  :hook
  (org-mode . org-indent-mode) ;; indent text
  :custom
  (org-edit-src-indentation 4)  ;; set src block autoindent to 4, not 2
  (org-return-follows-link t)   ;; sets RETURN key in org-mode to follow links
  )

(use-package org-journal
  :pin melpa-stable
  :ensure t
  :defer t
  :bind
  (("C-c C-j" . org-journal-new-entry))
  :init
  (setq org-journal-prefix-key "C-c j")
  :config
  (setq org-journal-dir "~/org/journal"))

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

(use-package monokai-pro-theme
  :ensure t
  :config
  (load-theme 'monokai-pro t))

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
  :custom
  (projectile-create-missing-test-files t)              ;;
  (projectile-run-use-comint-mode t)                    ;; Interactive run dialog when running projects inside emacs
  (projectile-project-search-path '("~/src" . 1))       ;; . 1 means to search only 1st subdir for projects
  :config
  (projectile-global-mode))

(use-package magit
  :defer
  :custom
  (magit-diff-refine-hunk (:quote all))  ;; Shows inline diff
  :config
  (define-key transient-map (kbd "<escape>") 'transient-quit-one)  ;; <esc> quits magit prompts
  )

(use-package markdown-mode)
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

(use-package clojure-mode
  :ensure t)

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

(use-package yasnippet
  :ensure t
  :hook ((text-mode
		  prog-mode
		  conf-mode
		  snippet-mode) . yas-minor-mode-on)
  :init
  (setq yas-snippet-dir "~/.emacs.d/snippets"))

(provide 'init)
