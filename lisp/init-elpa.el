;;; init-elpa.el --- Settings and helpers for package.el -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'package)
(require 'cl-lib)

;;; Install into separate package dirs for each Emacs version, to prevent bytecode incompatibility
(setq package-user-dir
      (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
                         user-emacs-directory))

;;; Standard package repositories
(setq package-archives '(("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("orgmode"      . "https://orgmode.org/elpa/")
                         ("nongnu"       . "https://elpa.nongnu.org/nongnu/")
                         ("tromey"       . "https://tromey.com/elpa/")
                         ("gnu"          . "https://elpa.gnu.org/packages/")
                         ("melpa"        . "https://melpa.org/packages/"))
      package-archive-prioritied '(("melpa-stable" . 30)
                                   ("orgmode"      . 30)
                                   ("nongnu"       . 20) 
                                   ("tromey"       . 20)
                                   ("gnu"          .  0)
                                   ("melpa"        .  0)))

(provide 'init-elpa)
;;; init-elpa.el ends here
