;;; init-frames-gui.el --- Non-TTY frames behavior -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Stop C-z frome 
(defun sanityinc/maybe-suspend-frame ()
  (interactive)
  (unless (and *is-macos* window-system)
    (suspend-frame)))

(global-set-key (kbd "C-z") 'sanityinc/maybe-suspend-frame)


;; Suppress GUI features
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)


;; Window size and features
(setq-default window-resize-pixelwise t
              frame-resize-pixelwise t)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-mode)
  (set-scroll-bar-mode nil))

(menu-bar-mode -1)

(provide 'init-frames-gui)
;;; init-frames-gui.el ends here
