;;; init-movement.el --- Movement helpers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Advises kill-region "C-w" so that if no region is selected, it kills/copies the current line.
(advice-add 'kill-region :before
			(lambda (&rest args)
			  "When called interactively with no active region, kill a single line instead."
			  (when (called-interactively-p 'interactive)
				(unless mark-active
				  (setq args (list (line-beginning-position)
								   (line-beginning-position 2)))))))  

(provide 'init-movement)
;;; init-movement.el ends here
