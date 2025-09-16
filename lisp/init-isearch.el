;;; init-isearch.el --- isearch settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun sanityinc/isearch-occur ()
  "Invoke `consult-line' from isearch."
  (interactive)
  (let ((query (if isearch-regexp
					 isearch-string
				 (regexp-quote isearch-string))))
	(isearch-update-ring isearch-string isearch-regexp)
	(let (search-nonincremental-instead)
	  (ignore-errors (isearch-done t t)))
	(consult-line query)))

;; Search back/forth for the symbol at point
;; see https://www.emacswiki.org/emacs/SearchAtPoint
(defun isearch-yank-symbol ()
  "*Put symbol at current point into search string."
  (interactive)
  (let ((sym (thing-at-point 'symbol)))
	(if sym
		(progn
		  (setq isearch-regexp t
				isearch-string (concat "\\_<" (regexp-quote sym) "\\_>")
				isearch-message (mapconcat 'isearch-text-char-description isearch-string "")
				isearch-yank-flag t))
	  (ding)))
  (isearch-search-and-update))


(use-package isearch
  :bind
  (("C-c r"    . isearch-forward-regexp)
   ("C-r"      . isearch-backward-regexp)
   ("C-c s"    . isearch-forward-symbol)
   ("C-s"      . isearch-backwards-symbol)
   ("C-o"      . sanityinc/isearch-occur)
   ("C-c C-o"  . sanityinc/isearch-occur)
   :map isearch-mode-map
   ("<M-down>" . isearch-ring-advance)
   ("<M-up>"   . isearch-ring-retreat)
   :map minibuffer-local-isearch-map
   ("<M-down>" . next-history-element)
   ("<M-up"    . previous-history-element))

  :init
  (setq-default isearch-allow-scroll t
				lazy-highlight-cleanup nil
				lazy-highlight-initial-delay 0))


;; Exits at the bottom of the marked text
(defun sanityinc/isearch-exit-other-end ()
  "Exit isearch, but at the other end of the search string.
This is useful when followed by an immediate kill."
  (interactive)
  (isearch-exit)
  (goto-char isearch-other-end))

(define-key isearch-mode-map [(control return)] 'sanityinc/isearch-exit-other-end)

(provide 'init-isearch)
;;; init-isearch.el ends here
