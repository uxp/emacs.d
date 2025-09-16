;;; init-perf-tuning.el --- Performance tuning -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; General performance tuning with the Garbage Collector Magic Hack
(use-package gcmh
  :init
  (setq gcmh-high-cons-threshold (* 128 1024 1024))
  :config
  (gcmh-mode 1)
  :hook ((after-init . (lambda ()
                         (gcmh-mode)
                         (diminish 'gcmh-mode)))))

(setq jit-lock-defer-time 0)

(provide 'init-perf-tuning)
;;; init-site-perf-tuning.el ends here
