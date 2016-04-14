;;;;
;; Cider
;;;;

;; Provide minibuffer documentation when writing to the REPL
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;; Go right to the repl buffer when it connects
(setq cider-repl-pop-to-buffer-on-connect t)

;; When there is a cider error, show its buffer and switch to it
(setq cider-show-error-buffer t)
(setq cider-auto-select-error-buffer t)

;; where to store cider history
(setq cider-repl-history-file "~/.emacs.d/cider-history")

;; wrap when navigating history
(setq cider-repl-wrap-history t)

;; enable paredit in the cider repl
(add-hook 'cider-repl-mode-hook 'paredit-mode)

;; use clojure mode for other extensions
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))
(add-to-list 'auto-mode-alist '("lein-env$" . clojure-mode))


;; Key Bindings for cider
(defun cider-start-http-server ()
  (interactive)
  (cider-load-current-buffer)
  (let ((ns (cider-current-ns)))
    (cider-repl-set-ns ns)
    (cider-interactive-eval (format "(println '(def server (%s/start))) (println 'server)" ns))
    (cider-interactive-eval (format "(def server (%s/start)) (println server)"))))

(defun cider-refresh ()
  (interactive)
  (cider-interactive-eval (format "(user/reset)")))

(defun cider-user-ns ()
  (interactive)
  (cider-repl-set-ns "user"))


(eval-after-load 'cider
  '(progn
     (define-key clojure-mode-map (kbd "C-c C-v") 'cider-start-http-server)
     (define-key clojure-mode-map (kbd "C-M-r") 'cider-refresh)
     (define-key clojure-mode-map (kbd "C-c u") 'cider-user-ns)
     (define-key cider-mode-map (kbd "C-c u") 'cider-user-ns)))
