;;; early-init.el --- Emacs 27+ pre-init config -*- lexical-binding: t -*-

;;; Documentation: (info "(emacs) Early Init File")
;;;                (info "(emacs) Package Installation")
;;; Commentary:

;; Emacs 27+ loads this file before (normally) calling
;; `package-initialize'. We use this file to suppress that automatic
;; behavior so that startup is consistent across Emacs versions.
;;
;; We set 'package-enable-at-startup' to 'nil' so that Emacs will not
;; automatically activate all installed packages before init.el (but
;; after reading this early-init.el)

;;; Code:
(setq package-enable-at-startup nil)

(setq tool-bar-mode nil
      menu-bar-mode t)

(setq initial-frame-alist
      '((width . 90)
        (height . 55)))

;; The scratch buffer is created before `user-unit-file` is evaluated, so if
;; you want `find-file` to start somewhere other than ~ it needs to be here in
;; your early-init file.
(defvar user-project-dir "~/Projects/")
(setq default-directory user-project-dir)

;; end early-init.el
