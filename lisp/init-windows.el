;;; init-windows.el --- Working with windows within frames -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package window
  :ensure nil
  :bind
  (;; Dont ask before killing a buffer.
   ([remap kill-buffer] . kill-this-buffer)))

;; Show window number when switching
(use-package switch-window
  :ensure t
  :defer t
  :config
  (setq switch-window-input-style 'minibuffer)
  (setq switch-window-increase 4)
  (setq switch-window-threshold 2)
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-qwerty-shortcuts
		'("a" "s" "d" "f" "g" "h" "i" "j" "k"))
  :bind
  (;; TODO: validate these
   ("C-x 0" . switch-window-then-delete)
   ("C-x 1" . switch-window-then-maximum)
   ("C-x 2" . switch-window-then-split-below)
   ("C-x 3" . switch-window-then-split-right)
   ("C-x o" . switch-window)
   ("C-x w" . switch-window-then-swap-buffer)
   ("C-<up>" . windmove-up)
   ("C-<down>" . windmove-down)
   ([remap other-window] . switch-window)))

;; Helps move buffers around
(use-package buffer-move
  :ensure t
  :defer t
  :bind
  (;; Steal VIM movement keys.
   ("C-S-h" . #'buf-move-left )
   ("C-S-j" . #'buf-move-down)
   ("C-S-k" . #'buf-move-up)
   ("C-S-l" . #'buf-moce-right)))



(provide 'init-windows)
;;; init-windows.el ends here
