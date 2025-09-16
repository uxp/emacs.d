(require 'init-example)

;; init.el --- -*- lexical-binding: t -*-

;; This config targets Emacs 30
;; Written by hplogsdon (https://gitlab.com/hplogdon/dotfiles)

;; Produce backtraces on error: helpful for startup issues
;;(setq debug-on-error t)

(let ((minver "29.1"))
  (when (version< emacs-version minver)
        (error "Emacs is too old.")))

;; Add the `lisp' directory, where we'll start to load individual modules.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))



;; Include it in the main init.el


(require 'init-benchmarking)

;; Helper functions

;; A number of helpful defines and functions


(setq *spell-check-support-enabled* nil) ;; Enable with 't if you prefer
(setq *is-macos* (eq system-type 'darwin))

(defun reload-user-init-file ()
  "Reload the init file"
  (interactive)
  (load-file user-init-file))

(defun find-init-file ()
  (interactive)
  (let ((this-init-file "~/.emacs.d/init.el"))
	(find-file this-init-file)))

(bind-key "C-c l" 'reload-init-file)

;; Adjust garbage collection threshold for early startup (see gcmh below)
(setq gc-cons-threshold (* 128 1024 1024))

;; Process performance tuning
(setq read-process-output-max (* 4 1024 1024))
(setq process-adaptive-read-buffering nil)

;; Bootstrapping

;; We call this bootstrapping, but we're already a ways into this thing. Here we attempt to load pre-package paths and utilities


;; Set user custom file
(setq custom-file (locate-user-emacs-file "custom.el"))

(require 'init-utils)

;; This must come before `init-elpa.el', as it may provide package.el
;; or equivalent functions
(require 'init-site-lisp)

(require 'init-elpa)

(require 'init-exec-path)

(require 'init-perf-tuning)

(require 'init-preload-local)

(require 'init-frame-hooks)

(require 'init-xterm)

(require 'init-themes)

(require 'init-macos)

(require 'init-frames-gui)

(require 'init-dired)

(require 'init-isearch)

(require 'init-grep)

(require 'init-uniquify)

(require 'init-ibuffer)

(require 'init-flymake)

(require 'init-xref)

(require 'init-eglot)

(require 'init-recentf)

(require 'init-minibuffer)

(require 'init-movement)

(require 'init-hippy-expand)

(require 'init-corfu)

(require 'init-windows)

(require 'init-sessions)

(require 'init-mmm)

(require 'init-editing-utils)

(require 'init-whitespace)

(require 'init-version-control)

(require 'init-git)

(require 'init-github)

(require 'init-gitlab)

(require 'init-projectile)

(require 'init-orgmode)

(require 'init-paredit)

(require 'init-lisp)

(require 'init-terminal)

(require 'init-claude)

;; Allow access from emacsclient 
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

(when (file-exists-p custom-file)
	(load custom-file))

;; Locales (setting them earlier doesn't work in X)
(require 'init-locales)

;; Load an optional `init-local.el' file containing untracked settings.
(require 'init-local nil t)

(provide 'init)

;; Local Variables:
;; coding: utf-8-unix
;; no-byte-compile: t
;; End:
;;; init.el ends here
