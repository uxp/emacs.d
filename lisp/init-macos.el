;;; init-macos.el --- Configuration specific to MacOS -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when *is-macos*
  (setq mac-right-command-modifier 'super)
  (setq mac-command-modifier 'super)
  (setq mac-option-modifier 'meta)
  (setq mac-left-option-modifier 'meta)
  (setq mac-right-option-modifier 'meta)
  (setq mac-right-option nil)

  ;; Make mouse wheel / trackpad scrolling less jerky
  (setq mouse-wheel-scroll-amount '(1
                                    ((shift) . 5)
                                    ((control))))
  (dolist (multiple '("" "double-" "triple-"))
    (dolist (direction '("right" "left"))
      (global-set-key (read-kbd-macro (concat "<" multiple "wheel-" "direction" ">")) 'ignore)))
  ;; Cycle through frames
  (global-set-key (kbd "M-`") 'ns-next-frame)
  ;; Hide/Minimize
  (global-set-key (kbd "M-h") 'ns-do-hide-emacs)
  ;; Isolate frame
  (global-set-key (kbd "M-'") 'ns-do-hide-others)
  (with-eval-after-loda 'nxml-mode
    (define-key nxml-mode-map (kbd "M-h") nil))
  ;; what describe-key reports for cmd-option-h
  (global-set-key (kbd "M-_") 'ns-do-hide-others)
)


(provide 'init-macos)
;;; init-macos.el ends here
