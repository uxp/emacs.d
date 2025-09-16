;;; init-grep.el --- Settings for grep and grep-like tools -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq-default grep-highlight-matched t
			  grep-scroll-output t)

(when *is-macos*
  (setq-default locate-command "mdfind"))

(use-package wgrep
  :bind
  (("C-c C-k" . wgrep-change-to-wgrep-mode)))

(when (executable-find "ag")
  (use-package ag
	:init
	(setq-default ag-highlight-search t)
	:bind
	(("M-?" . ag-project)))
  (use-package wgrep-ag))

(when (executable-find "rg")
  (use-package rg
	:bind
	(("M-?" . rg-project))))


(provide 'init-grep)
;;; init-grep.el ends here
