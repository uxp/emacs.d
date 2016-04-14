;;
;; Packages
;;

(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
	     '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Load and activate emacs pacakges. Do this first so that the
;; packages are loaded before we start to modify them
;; Also sets the load path
(package-initialize)

;; Download the ELPA archive description if needed.
;; THis informs Emacs about the latest versions of all packages
;; and makes them available for download.
(when (not package-archive-contents)
 (package-refresh-contents))


;; The packages
(defvar packages
  '(;; Makes writing lisp expressions much, much easier
    paredit

    ;; key bindings and highlighting for Clojure
    clojure-mode
    clojure-mode-extra-font-locking

    ;; integration with the Clojure REPL
    cider

    ;; colorful parens 
    rainbow-delimiters

    ;; project navigation
    projectile

    ;; edit tags like sexps
    tagedit

    ;; integration with Git
    magit))

(dolist (p packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Non-Packaged elisp files from ~/.emacs.d/vendor.
(add-to-list 'load-path "~/.emacs.d/vendor")

;; Turn off the menu bar at the top
(menu-bar-mode -1)

;; show line numbers
(global-linum-mode)

;; highlight matching parens
(show-paren-mode 1)

;; highlight current line
(global-hl-line-mode 1)

;; Turn off the toolbar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Color themes
;; 
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")

(set-face-attribute 'default nil :height 140)

(setq ;; makes killing/yanking interact with the clipboard
  x-select-enable-clipboard t

  ;; wat
  x-select-enable-primary t

  ;; save clipboard strings into the kill ring before replacing them.
  ;; when one selects something in another program to paste into Emacs,
  ;; but kills something in Emacs before actually pasting it,
  ;; this selection is gone unless this variable is non-nil
  save-interprogram-paste-before-kill t

  ;; shows all options when running apropos.
  apropos-do-all t

  ;; Mouse yank commands yank at point, instead of click.
  mouse-yank-at-point t)

;; No blink cursor
(blink-cursor-mode 0)

;; set full path in titlebar
(setq-default frame-title-format "%b (%f)")

;; no bell
(setq ring-bell-function 'ignore)

;; Automatically load paredit when editing a lisp file
;; 
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of List code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'clojure-mode-hook          #'enable-paredit-mode)

;; eldoc-mode shows documentation in the minibuffer when writing code
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-mode-hook       'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook       'turn-on-eldoc-mode)

;; Useful for camelCase tokens (java interop)
(add-hook 'clojure-mode-hook    'subword-mode)

;; A little more syntax highlighting for clojure
(require 'clojure-mode-extra-font-locking)

;; change all yes/no questions to y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; dont create ~ lockfiles
(setq create-lockfiles nil)

;; Go to scratch buffer on startup
(setq inhibit-startup-message t)

;; only use soft tabs
(setq-default indent-tabs-mode nil)

;; rainbows!!!
;(global-rainbow-delimiters-mode t)

