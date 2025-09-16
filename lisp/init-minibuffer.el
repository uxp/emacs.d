;;; init-minibuffer.el --- Example -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun switch-to-minibuffer ()
  "Switch to minibuffer."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))

(defun kill-this-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(bind-key "M-m" 'switch-to-minibuffer)
(bind-key "C-c C-k" 'kill-this-buffer)


(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
