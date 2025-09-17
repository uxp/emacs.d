;;; --- Description statement

;;; init.el --- Local Initialization File -*- lexical-binding: t -*-
;;; Commentary:
;;;   This config targets Emacs 30, and is automatically generated via
;;;   Org-Mode Tangled file README.org
;;;
;;;   Written by hplogsdon (https://gitlab.com/hplogdon/dotfiles)
;;;
;;;   Do not edit this by hand.
;;;
;;; Code:

;; [[file:../Emacs.org::*Header and Guard Statements][Header and Guard Statements:2]]
;; Produce backtraces on error: helpful for startup issues
(setq debug-on-error t
      debug-on-quit nil
      debug-on-signal nil)

(let ((minver "29.1"))
  (when (version< emacs-version minver)
        (error "Emacs is too old.")))

;; Add the `lisp' directory
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;; Header and Guard Statements:2 ends here

;; [[file:../Emacs.org::*Benchmarking][Benchmarking:1]]
;;; --- Measure startup time and require times

(defun sanityinc/time-subtract-millis (b a)
  (* 1000.0 (float-time (time-subtract b a))))


(defvar sanityinc/require-times nil
  "A list of (FEATURE LOAD-START-TIME LOAD-DURATION).
LOAD-DURATION is the time taken in milliseconds to load FEATURE.")


(defun sanityinc/require-times-wrapper (orig feature &rest args)
  "Note in `sanityinc/require-times' the time taken to require each feature."
  (let* ((already-loaded (memq feature features))
		 (require-start-time (and (not already-loaded) (current-time))))
	(prog1
		(apply orig feature args)
	  (when (and (not already-loaded) (memq feature features))
		(let ((time (sanityinc/time-subtract-millis (current-time) require-start-time)))
		  (add-to-list 'sanityinc/require-times
					   (list feature require-start-time time)
					   t))))))


(advice-add 'require :around 'sanityinc/require-times-wrapper)

(define-derived-mode sanityinc/require-times-mode tabulated-list-mode "Require-Times"
  "Show times taken to `require' package"
  (setq tabulated-list-format
		[("Start time (ms)" 20 sanityinc/require-times-sort-by-start-time-pred)
		 ("Feature" 30)
		 ("Time (ms)" 12 sanityinc/require-times-sort-by-load-time-pred)])
  (setq tabulated-list-sort-key (cons "Start time (ms)" nil))
  (setq tabulated-list-padding 2)
  (setq tabulated-list-entries #'sanityinc/require-times-tabulated-list-entries)
  (tabulated-list-init-header)
  (when (fboundp 'tablist-minor-mode)
	(tablist-minor-mode)))


(defun sanityinc/require-times-sort-by-start-time-pred (entry1 entry2)
  (< (string-to-number (elt (nth 1 entry1) 0))
	 (string-to-number (elt (nth 1 entry2) 0))))


(defun sanityinc/require-times-sort-by-load-time-pred (entry1 entry2)
  (> (string-to-number (elt (nth 1 entry1) 0))
	 (string-to-number (elt (nth 1 entry2) 0))))


(defun sanityinc/require-times-tabulated-list-entries ()
  (cl-loop for (feature start-time millis) in sanityinc/require-times
		   with order = 0
		   do (cl-incf order)
		   collect (list order
						 (vector
						  (format "%.3f" (sanityinc/time-subtract-millis start-time before-init-time))
						  (symbol-name feature)
						  (format "%.3f" millis)))))


(defun sanityinc/require-times ()
  "Show a tabular view of how long various libraries took to load.")


(defun sanityinc/show-init-time ()
  (message "init completed in %.2fms"
		   (sanityinc/time-subtract-millis after-init-time before-init-time)))

(add-hook 'after-init-hook 'sanityinc/show-init-time)
;; Benchmarking:1 ends here

;; [[file:../Emacs.org::*Helper functions][Helper functions:1]]
(setq *spell-check-support-enabled* nil) ;; Enable with 't if you prefer

(setq *is-macos* (eq system-type 'darwin))
(setq *is-windows* (memq system-type '(windows-nt ms-dos cygwin)))
(setq *is-linux* (eq system-type 'gnu/linux))

(defun hplogsdon/reload-user-init-file ()
  "Reload the init file"
  (interactive)
  (load-file user-init-file))

(defun hplogsdon/find-init-file ()
  (interactive)
  (let ((this-init-file "~/.emacs.d/init.el"))
	(find-file this-init-file)))

(bind-key "C-c l" 'hplogsdon/reload-user-init-file)

;; Adjust garbage collection threshold for early startup (see gcmh below)
(setq gc-cons-threshold (* 128 1024 1024))

;; Process performance tuning
(setq read-process-output-max (* 4 1024 1024))
(setq process-adaptive-read-buffering nil)
;; Helper functions:1 ends here

;; [[file:../Emacs.org::*Bootstrapping][Bootstrapping:1]]
;; Set user custom file
(setq custom-file (locate-user-emacs-file "custom.el"))
;; Bootstrapping:1 ends here

;; [[file:../Emacs.org::*Package Management][Package Management:1]]
;; Set straight.el default config before bootstrapping
(setq straight-use-package-by-default t                 ; use-package defaults to straight.el
      straight-recipes-gnu-elpa-use-mirror t            ; use straight's mirror of elpa
      straight-built-in-pseudo-packages                 ; dont auto-fetch these builtins
      '(dired
        emacs-lisp-mode
        inferior-lisp
        isearch
        use-package
        uniquify
        vc
        which-function-mode
        ))

;;; Standard package repositories
(setq package-user-dir (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
                                                 user-emacs-directory)
      package-archives '(("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("orgmode"      . "https://orgmode.org/elpa/")
                         ("nongnu"       . "https://elpa.nongnu.org/nongnu/")
                         ("tromey"       . "https://tromey.com/elpa/")
                         ("gnu"          . "https://elpa.gnu.org/packages/")
                         ("melpa"        . "https://melpa.org/packages/"))
      package-archive-priorities '(("melpa-stable" . 30)
                                   ("orgmode"      . 30)
                                   ("nongnu"       . 20) 
                                   ("tromey"       . 20)
                                   ("gnu"          .  0)
                                   ("melpa"        .  0)))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name 
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
       (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
          "https://radian-software.github.io/straight.el/install.el"
          'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq use-package-verbose t)
(straight-use-package 'use-package)

(require 'cl-lib)
;; Package Management:1 ends here

;; [[file:../Emacs.org::*Package helpers][Package helpers:1]]
;;; --- Package Helpers

(use-package diminish
  :ensure t)
;; Package helpers:1 ends here

;; [[file:../Emacs.org::*Helper functions and commands][Helper functions and commands:1]]
;;; --- Emacs-Lisp helper functions and commands

;; Delete the current file
(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))


;; Rename the current file
(if (fboundp 'rename-visited-file)
    (defalias 'rename-this-file-and-buffer 'rename-visited-file)
  (defun rename-this-file-and-buffer (new-name)
    "Renames both current buffer and file it's visiting to NEW-NAME."
    (interactive "New name: ")
    (let ((name (buffer-name))
          (filename (buffer-file-name)))
      (unless filename
        (error "Buffer '%s' is not visiting a file!" name))
      (progn
        (when (file-exists-p filename)
          (rename-file filename new-name 1))
        (set-visited-file-name new-name)
        (rename-buffer new-name)))))
;; Helper functions and commands:1 ends here

;; [[file:../Emacs.org::*Lisp directory (aka, `vendor')][Lisp directory (aka, `vendor'):1]]
;;; --- Support elisp manually installed in the site-lisp dir

;; This must come before `elpa', as it may provide package.el
;; or equivalent functions

;; Set load path
(require 'cl-lib)

(defun sanityinc/add-subdirs-to-load-path (parent-dir)
  "Add every non-hidden subdir of PARENT-DIR to `load-path'."
  (let ((default-directory parent-dir))
    (setq load-path
          (append
           (cl-remove-if-not
            #'file-directory-p
            (directory-files (expand-file-name parent-dir) t "^[^\\.]"))
           load-path))))

;; Add both site-lisp and its immediate subdirs to `load-path'
(let ((site-lisp-dir (expand-file-name "site-lisp/" user-emacs-directory)))
  (push site-lisp-dir load-path)
  (sanityinc/add-subdirs-to-load-path site-lisp-dir))

;;; Utilities for grabbing upstream libs
(defun site-lisp-dir-for (name)
  (expand-file-name (format "site-lisp/%s" name) user-emacs-directory))

(defun site-lisp-library-el-path (name)
  (expand-file-name (format "%s.el" name (site-lisp-dir-for name))))

(defun download-site-lisp-module (name url)
  (let ((dir (site-lisp-dir-for name)))
    (message "Downloading %s from %s" name url)
    (unless (file-directory-p dir)
      (make-directory dir t))
    (add-to-list 'load-path dir)
    (let ((el-file (site-lisp-library-el-path name)))
      (url-copy-file url el-file t nil)
      el-file)))

(defun ensure-lib-from-url (name url)
  (unless (site-lisp-library-loadable-p name)
    (byte-compile-file (download-site-lisp-module name url))))

(defun site-lisp-library-loadable-p (name)
  "Return whether or not the library `name' can be loaded from a source file
under ~/.emacs.d/site-lisp/NAME"
  (let ((f (locate-library (symbol-name name))))
    (and f (string-prefix-p (file-name-as-directory (site-lisp-dir-for name)) f))))
;; Lisp directory (aka, `vendor'):1 ends here

;; [[file:../Emacs.org::*Exec Path][Exec Path:1]]
;;; --- Setup exec-path to help Emacs find packages

(when (or (memq window-system '(mac ns x pgtk))
          (unless (memq system-type '(ms-dos windows-nt))
            (daemonp)))
  (use-package exec-path-from-shell
    :ensure t
    :pin melpa-stable
    :config
    (exec-path-from-shell-initialize)
    (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH"))
      (add-to-list 'exec-path-from-shell-variables var))))
;; Exec Path:1 ends here

;; [[file:../Emacs.org::*Performance tuning][Performance tuning:1]]
;;; --- Performance tuning

;; General performance tuning with the Garbage Collector Magic Hack
(use-package gcmh
  :ensure t
  :demand t
  :diminish (gcmh-mode)
  :config
  (gcmh-mode 1)
  :init
  (setq gcmh-idle-delay 'auto)
  (setq gcmh-auto-idle-delay-factor 10)
  (setq gcmh-high-cons-threshold (* 128 1024 1024)))

(setq jit-lock-defer-time 0)
;; Performance tuning:1 ends here

;;; --- Provide specific hooks for GUI/TTY frame creation
(defvar after-make-console-frame-hooks '()
  "Hooks to run after creating a new TTY frame")

(defvar after-make-window-system-frame-hooks '()
  "Hooks to run after creating a new window-system frame")

(defun run-after-make-frame-hooks (frame)
  "Run configured hooks in response to the newly-created FRAME
Selectively runs either `after-make-console-frame-hooks' or
`after-make-window-system-frame-hooks'."
  (with-selected-frame frame
    (run-hooks (if window-system
                   'after-make-window-system-frame-hooks
                 'after-make-console-frame-hooks))))

(add-hook 'after-make-frame-functions 'run-after-make-frame-hooks)

(defconst sanityinc/initial-frame (selected-frame)
  "The frame (if any) active during Emacs initialization.")

(add-hook 'after-init-hook
          (lambda () (when sanityinc/initial-frame
                  (run-after-make-frame-hooks sanityinc/initial-frame))))

;;; --- Integrate with terminals such as xterm

(global-set-key [mouse-4] (lambda () (interactive) (scroll-down 1)))
(global-set-key [mouse-5] (lambda () (interactive) (scroll-up 1)))

(autoload 'mwheel-install "mwheel")

(defun sanityinc/console-frame-setup ()
  (xterm-mouse-mode 1)
  (mwheel-install))


(add-hook 'after-make-console-frame-hooks 'sanityinc/console-frame-setup)

;;; --- Theming

(use-package monokai-pro-theme
  :ensure t
  :init
  (load-theme 'monokai-pro t))

(use-package nerd-icons)

(use-package nerd-icons-completion
  :straight
  (nerd-icons-completion :type git :host github :repo "rainstormstudio/nerd-icons-completion")

  :hook
  ((marginalia-mode . nerd-icons-completion-marginalia-setup))

  :config
  (nerd-icons-completion-mode))

(use-package nerd-icons-dired
  :straight
  (nerd-icons-dired :type git :host github :repo "rainstormstudio/nerd-icons-dired")

  :hook
  ((dired-mode . nerd-icons-dired-mode)))

(use-package nerd-icons-ibuffer
  :straight
  (nerd-icons-ibuffer :type git :host github :repo "seagle0128/nerd-icons-ibuffer")

  :hook
  ((ibuffer-mode . nerd-icons-ibuffer-mode)))

(use-package nerd-icons-corfu
  :straight
  (nerd-icons-corfu :type git :host github :repo "LuigiPiucco/nerd-icons-corfu")

  :autoload nerd-icons-corfu-formatter
  :after corfu

  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;;; Configuration specific to MacOS

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
  (with-eval-after-load 'nxml-mode
    (define-key nxml-mode-map (kbd "M-h") nil))
  ;; what describe-key reports for cmd-option-h
  (global-set-key (kbd "M-_") 'ns-do-hide-others))

;;; --- Non-TTY frames behavior

;; Stop C-z frame
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
  (set-scroll-mode nil))

(menu-bar-mode -1)

;;; --- Dired customization

(use-package dired
  :ensure nil
  :defer t
  :hook
  ((dired-mode . dired-hide-details-mode))
  :config
  (require 'dired-x)
  (setq dired-omit-files "^\\.[^.].*$")
  (setq dired-recursive-copies 'always)
  (setq dired-create-destination-dirs 'ask)
  (setq dired-clean-confirm-killing-deleted-buffers nil)
  (setq dired-make-directory-clickable t)
  (setq dired-mouse-drag-files t)
  (setq dired-kill-when-opening-new-dired-buffer t)
  (when *is-macos*
    (let ((gls (executable-find "gls")))
      (if gls
	  (setq insert-directory-program gls
		dired-use-ls-dired t
		dired-listing-switches "-aBhl --group-directories-first")
	(setq dired-use-ls-dired nil)))))

;;; --- isearch settings

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
   ("C-c o"    . sanityinc/isearch-occur)
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

;;; --- Settings for grep and grep-like tools

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

;;; --- Configure uniquification of buffer name

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'reverse
        uniquify-separator " • "
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re "^\\*"))

;;; --- iBuffer settings

(use-package ibuffer
  :ensure nil
  :bind (("C-x C-b" . ibuffer))
  :commands (ibuffer-current-buffer
             ibuffer-find-file
             ibuffer-do-sort-by-alphabetic)
  :preface
  (defvar protected-buffers '("*scratch*" "*Messages*")
	"Buffers that cannot be killed")
  (defun hplogsdon/protected-buffers ()
	"Protect some buffers from being killed."
	(dolist (buffer protected-buffers)
	  (with-current-buffer buffer
		(emacs-lock-mode 'kill))))
  :init
  (setq ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold)))
  (setq ibuffer-formats '((mark modified read-only locked
                                " " (icon 2 2 :left :elide) (name 18 18 :left :elide)
                                " " (size 9 -1 :right)
                                " " (mode 16 16 :left :elide)
                                " " filename-and-process)
                          (mark modified read-only vc-status-mini
                                " " (name 22 22 :left :elide)
                                " " (size-h 9 -1 :right)
                                " " (mode 14 14 :left :elide)
                                " " (vc-status 12 12 :left)
                                " " vc-relative-file)
                          (mark " " (name 16 -1) " " filename)))
  (setq ibuffer-saved-filter-groups '(("default"
                                       ("org" (or (mode .org-mode) (name . "^\\Org Mode")))
                                       ("emacs" (or (name . "^\\*scratch\\*$") (name . "\\*Messages\\*$")))
                                       ("dired" (mode . dired-mode))
                                       ("terminal" (name . "^\\*Help\\*$")))))
  (hplogsdon/protected-buffers)
  :config
  (add-hook 'ibuffer-mode-hook
             (lambda ()
               (ibuffer-switch-to-saved-filter-groups "default")
               (ibuffer-update nil t)
               (ibuffer-auto-mode 1)))


  (setq ibuffer-show-empty-filter-groups nil))

;;; --- Configure flymake global behavior

(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
              ("C-c l" . flymake-show-buffer-diagnostics)
              ("C-c n" . flymake-goto-next-error)
              ("C-c p" . flymake-goto-prev-error)
              ("C-c c" . flymake-start)))

;;; --- Cross Reference (xref) configuration

(use-package xref
  :straight t
  :defer t
  :bind (("s-[" . #'xref-go-back)
         ("s-]" . #'xref-go-forward)
         ("C-c r" . #'xref-find-references)
         ("C-c d" . #'xref-find-definitions))
  :config (add-to-list 'xref-prompt-for-identifier #'xref-find-references 'append)
  :custom
  (xref-auto-jump-to-first-xref t))

;;; --- Settings for tracking recent files

(use-package recentf
  :defer t
  :init (recentf-mode)
  :config
  (setq recentf-max-saved-items 200
        recentf-auto-cleanup 300
        recentf-exclude (list "\\.git/.*\\'"  ; Git contents
                              "/elpa/.*\\'"   ; Package files
                              ".*\\.gz\\'"
                              "TAGS"
                              (concat package-user-dir "/.*-autoloads\\.el\\'")
                              "ido.last")))

;;;  --- 

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

;;; --- Day-to-Day editing helpers

;; Advises kill-region "C-w" so that if no region is selected, it kills/copies the current line.
(advice-add 'kill-region :before
	    (lambda (&rest args)
	      "When called interactively with no active region, kill a single line instead."
	      (when (called-interactively-p 'interactive)
		(unless mark-active
		  (setq args (list (line-beginning-position)
				   (line-beginning-position 2)))))))

;;; --- Version control support

;; program-specific version control packages are configured separately.
;; see `git', for example

(use-package diff-hl
  :ensure t
  :after (dired)
  :hook ((prog-mode  . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode)
         (after-init . global-diff-hl-mode))
  :init
  (defconst hplogsdon/diff-hl-mode-hooks '(emacs-lisp-mode-hook
                                           conf-space-mode-hook ; .tmux.conf
                                           markdown-mode-hook
                                           css-mode-hook
                                           web-mode-hook
                                           sh-mode-hook
                                           python-mode-hook
                                           yaml-mode-hook ; tmuxp yaml configs
                                           c-mode-hook)
	"List of hook of major modes in which `diff-hl-mode' should be enabled.")
  (dolist (hook hplogsdon/diff-hl-mode-hooks)
    (add-hook hook #'diff-hl-flydiff-mode))
  
  :config
  (with-eval-after-load 'magit
	  (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
	  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

  :custom
  (diff-hl-disable-on-remote t)
  (diff-hl-margin-symbols-alist
   '((insert . " ")
     (delete . " ")
     (change . " ")
     (unknown . "?")
     (ignored . "i"))))

;; TODO: diff-hl-hydra ?
;; (use-package diff-hl-hydra
;;   :after (hydra))

(use-package vc
  :bind (("C-x v =" . hplogsdon/vc-diff)
		 ("C-x v H" . vc-region-history)) ;; new command in emacs 25.x
  :config
  (defun hplogsdon/vc-diff (no-whitespace)
	  "Call `vc-diff' as usual if buffer is not modified.
  If the buffer is modified (yet to be saved, dirty) call
  `diff-buffer-with-file'. If NO-WHITESPACE is non-nill, ignore
  all whitespace when doing diff."
	  (interactive "P")
	  (let* ((no-ws-switch '("-w"))
		     (vc-git-diff-switches (if no-whitespace
			  					 no-ws-switch
								   vc-git-diff-switches))
		   (vc-diff-switches (if no-whitespace
								 no-ws-switch
							   vc-diff-switches))
		   (diff-switches (if no-whitespace
							  no-ws-switch
							vc-diff-switches))
		   ;; set `current-prefix-arg' no nil so that the HISTORIC arg of
		   ;; `vc-diff' stays nil.
		   current-prefix-arg)
	  (if (buffer-modified-p)
		  (diff-buffer-with-file (current-buffer))
		(call-interactively #'vc-diff)))))

;;; --- Github integration

;; yagist
;; (use-package yagist)

;; github-clone
;; (use-package github-clone)

;; github-review
;; (use-package github-review)

;; todo: flymake-actionlint
;; todo: forge
;; todo: bug-reference-github

;;; --- Gitlab Integration

;; [[file:../Emacs.org::*orgmode][orgmode:1]]
;;; --- Org Mode Configuration

(use-package org
  :pin "orgmode"
  :mode ("\\.org\\'" . org-mode)
  :defer t
  :bind (("C-c a" . org-agenda)
		 ("C-c c" . org-capture)
		 ;("C-c j" . org-journal)
		 (:map org-mode-map
			   (("M-p" . outline-previous-visible-heading)
				("M-n" . outline-next-visible-heading)
				("C-c C-p" . eaf-org-export-to-pdf-and-open)
				("C-c ;" . nil))))

  :custom
  (org-return-follows-link t)
  (org-export-backends (quote (ascii html latex md odt)))
  (org-confirm-babel-evaluate 'nil)
  (org-deadline-warning-days 7)
  (org-agenda-window-setup 'other-window)
  (org-babel-load-languages
   '((emacs-lisp . t)
	 (python . t)
	 (dot . t)))
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
	 (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))

  :custom-face
  (org-agenda-current-time ((t (:foreground "spring green"))))

  :config
  (unless (version< org-version "9.2")
	(require 'org-tempo))
  (when (or (file-directory-p "~/org/agenda") (file-directory-p "~/org/journal"))
    (setq org-agenda-files (list "~/org/agenda" "~/org/journal"))))

(use-package org-journal
  :bind (("C-c j n" . org-journal-new-entry)
	   ("C-c j t" . org-journal-today))

  :custom
  (org-journal-date-prefix "#+TITLE: ")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-dir "~/org/journal")
  (org-journal-date-format "%Y-%m-%d")

  :config
  (defun org-journal-today ()
	(interactive)
	(org-journal-new-entry t)))
;; orgmode:1 ends here

;;; --- Paredit mode

(use-package paredit
  :ensure t
  :diminish (paredit-mode))
;;  :hook ((lisp-mode             . paredit-mode)
;;	 (cider-mode            . paredit-mode)
;;	 (cider-repl-mode       . paredit-mode)
;;     (clojure-mode          . paredit-mode)
;;	 (emacs-lisp-mode       . paredit-mode))

;;; --- Emacs lisp settings, and common config for other lisps

(defun hplogsdon/eval-last-sexp-or-region (prefix)
  "Eval region from BEG to END if active, otherwise the last sexp."
  (interactive "P")
  (if (and (mark) (use-region-p))
	  (eval-region (min (point) (mark)) (max (point) (mark)))
	(pp-eval-last-sexp prefix)))

;; TODO: look into `litable' as an alternative
(defun hplogsdon/elisp-eval-and-comment-output ()
  "Add the output of the sexp as a comment after the sexp"
  (interactive)
  (save-excursion
	(end-of-line)
	(condition-case nil
		(printc (concat " ; -> " (pp-to-string (eval (preceding-sexp))))
				(current-buffer))
	  (error (message "Invalid expression")))))

(defun hplogsdon/elisp-eval-region ()
  (interactive)
  (if (region-active-p)
	  (progn
		(eval-region (region-beginning)
					 (region-end))
		(deactivate-mark))
	(eval-expression)))

(defun hplogsdon/elisp-headerize ()
  "Adds a header and footer to an elisp buffer for Flycheck"
  (interactive)
  (let ((fname (if (buffer-file-name)
				   (file-name-nondirectory (buffer-file-name))
				 (error "This buffer is not visiting a file"))))
	(save-excursion
	  (goto-char (point-min))
	  (insert ";;; " fname " --- Description -*- lexical-binding: t -*-\n"
			  ";;; Commentary:\n"
			  ";;; Code:\n\n")
	  (goto-char (point-max))
	  (insert ";;; " fname " ends here\n"))))

(defun hplogsdon/elisp-register-elc-delete-on-save ()
  "If you're saving an elisp file, the .elc is likely invalid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
			'(lambda ()
			   (when (file-exists-p (concat buffer-file-name "c"))
				 (delete-file (concat buffer-file-name "c"))))))

(use-package emacs-lisp-mode
  :defer t
  :hook ((emacs-lisp-mode . outline-minor-mode)
	       (emacs-lisp-mode . reveal-mode)
         (emacs-lisp-mode . eldoc-mode))
  :bind (("C-x e" . hplogsdon/elisp-eval-and-comment-output))
  :mode (("\\.el$" . emacs-lisp-mode))
  :init
  (setq initial-major-mode 'emacs-lisp-mode)
  (hplogsdon/elisp-register-elc-delete-on-save)

  :config
  (eldoc-mode 1)
  (hplogsdon/elisp-register-elc-delete-on-save))


(use-package color-identifiers-mode
  :ensure t
  :hook ((emacs-lisp-mode . color-identifiers-mode)))

;; [[file:../Emacs.org::*which-key][which-key:1]]
;;; --- Which Key

(use-package which-key
  :ensure t
  :diminish ""
  :custom
  (which-key-mode t))

(use-package which-key-posframe
  :config
  (set-face-attribute 'which-key-posframe nil :background "wheat1")
  :custom
  (which-key-posframe-mode t)
  (which-key-posframe-poshandler 'posframe-poshandler-frame-bottom-left-corner))

(which-function-mode t)
;; which-key:1 ends here

;; [[file:../Emacs.org::*Footer][Footer:1]]
;; Allow access from emacsclient 
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

(when (file-exists-p custom-file)
	(load custom-file))


;;; Configure default locales
(defun sanityinc/locale-var-encoding (v)
  "Returning the encoding portion of the locale string V, or nil if missing."
  (when v
    (save-match-data)
      (let ((case-fold-search t))
        (when (string-match "\\.\\([^.]*\\)\\'" v)
          (intern (downcase (match-string 1 v)))))))

(dolist (varname '("LC_ALL" "LANG" "LC_CTYPE"))
  (let ((encoding (sanityinc/locale-var-encoding (getenv varname))))
    (unless (memq encoding '(nil utf8 utf-8))
      (message "Warning: non-UTF8 encoding in environment variable %s may cause interop problems with this Emacs configuration." varname))))

;; Set UTF-8 as the default encoding
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))       
(prefer-coding-system 'utf-8-unix)
(setq locale-coding-system 'utf-8
      coding-system-for-read 'utf-8
      coding-system-for-write 'utf-8)
(unless *is-windows*
  (set-selection-coding-system 'utf-8))


(provide 'init)
;; Local Variables:
;; coding: utf-8-unix
;; no-byte-compile: t
;; End:
;;; init.el ends here
;; Footer:1 ends here
