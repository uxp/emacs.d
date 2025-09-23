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

;; [[file:README.org::*Header and Guard Statements][Header and Guard Statements:2]]
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

;; [[file:README.org::*Benchmarking][Benchmarking:1]]
;;; --- Measure startup time and require times

(defun sanityinc/time-subtract-millis (b a)
  "Subtract time A from time B in milliseconds."
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
  "Sorting function predicate for `sanityinc/require-times-mode' comparing ENTRY1 and ENTRY2 by start time."
  (< (string-to-number (elt (nth 1 entry1) 0))
     (string-to-number (elt (nth 1 entry2) 0))))


(defun sanityinc/require-times-sort-by-load-time-pred (entry1 entry2)
  "Sorting function predicate for `sanityinc/require-times-mode' comparing ENTRY1 and ENTRY2 by load time."
  (> (string-to-number (elt (nth 1 entry1) 0))
     (string-to-number (elt (nth 1 entry2) 0))))


(defun sanityinc/require-times-tabulated-list-entries ()
  "Show require times of all modules in a table format."
  (cl-loop for (feature start-time millis) in sanityinc/require-times
           with order = 0
           do (cl-incf order)
           collect (list order
                         (vector
                          (format "%.3f" (sanityinc/time-subtract-millis start-time before-init-time))
                          (symbol-name feature)
                          (format "%.3f" millis)))))


(defun sanityinc/require-times ()
  "Show a tabular view of how long various libraries took to load."
  (interactive)
  (with-current-buffer (get-buffer-create "*Require Times*")
    (sanityinc/require-times-mode)
    (tabulated-list-revert)
    (display-buffer (current-buffer))))

(defun sanityinc/show-init-time ()
  "Common function that prints the initialization time of Emacs."
  (message "init completed in %.2fms"
           (sanityinc/time-subtract-millis after-init-time before-init-time)))

(add-hook 'after-init-hook 'sanityinc/show-init-time)
;; Benchmarking:1 ends here

;; [[file:README.org::*Defines][Defines:1]]
;;; --- Helpful defines and functions
(setq *spell-check-support-enabled* nil) ;; Enable with 't if you prefer

(setq *is-macos* (eq system-type 'darwin))
(setq *is-windows* (memq system-type '(windows-nt ms-dos cygwin)))
(setq *is-linux* (eq system-type 'gnu/linux))

;; Adjust garbage collection threshold for early startup (see gcmh below)
(setq gc-cons-threshold (* 128 1024 1024))

;; Process performance tuning
(setq read-process-output-max (* 4 1024 1024))
(setq process-adaptive-read-buffering nil)
;; Defines:1 ends here

;; [[file:README.org::*Bootstrapping][Bootstrapping:1]]
;;; --- Bootstrap stuff.
;; Set user custom file
(setq custom-file (locate-user-emacs-file "custom.el"))
;; Bootstrapping:1 ends here

;; [[file:README.org::*Package Management][Package Management:1]]
;; Set straight.el default config before bootstrapping
(setq straight-use-package-by-default t                 ; use-package defaults to straight.el
      straight-recipes-gnu-elpa-use-mirror t            ; use straight's mirror of elpa
      straight-built-in-pseudo-packages                 ; dont auto-fetch these builtins
      '(dired
        emacs-lisp-mode
        inferior-lisp
        isearch
        lsp-mode
        mwheel
        nxml-mode
        use-package
        uniquify
        vc
        which-function-mode))


;; The following is probably unused, but I'm not deleting it yet.
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

;; [[file:README.org::*Package helpers][Package helpers:1]]
;;; --- Package Helpers

(use-package diminish
  :ensure t)
;; Package helpers:1 ends here

;; [[file:README.org::*Init file helpers][Init file helpers:1]]
;;; --- Emacs-Lisp helper functions and commands

(defun hpl/reload-user-init-file ()
  "Reload the init file."
  (interactive)
  (load-file user-init-file))

(defun hpl/find-init-file ()
  "Opens emacs.d/init.el in the buffer."
  (interactive)
  (let ((this-init-file "~/.emacs.d/init.el"))
    (find-file this-init-file)))

(bind-key "C-c C-<f5>" 'hpl/reload-user-init-file)
;; Init file helpers:1 ends here

;; [[file:README.org::*Delete files][Delete files:1]]
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
;; Delete files:1 ends here

;; [[file:README.org::*DWIM Capitalization][DWIM Capitalization:1]]
;; Capitalize current word or all words in region
(defun hpl/capitalization-dwim ()
  "Capitalize all words in region or point."
  (interactive)
  (if (region-active-p)
      (save-excursion (capitalize-region (region-beginning) (region-end)))
    (capitalize-word 1)))

;; Upcase current word or region
(defun hpl/upcase-dwim ()
  "Upcase all words in region or point."
  (interactive)
  (if (region-active-p)
      (save-excursion (upcase-region (region-beginning) (region-end)))
    (upcase-word 1)))

(defun hpl/downcase-dwim ()
  "Downcase all words in region or point."
  (interactive)
  (if (region-active-p)
      (save-excursion (downcase-region (region-beginning) (region-end)))
    (downcase-word 1)))

;; bind keys from table
;; DWIM Capitalization:1 ends here

;; [[file:README.org::*Duplication][Duplication:1]]
(defun hpl/duplicate-current-line-or-region (arg)
  "Duplicate the current line or region ARG times.
If there is no region, the current line will be duplicated."
  (interactive "p")
  (save-excursion
    (if (region-active-p)
        (hpl/duplicate-region arg)
      (hpl/duplicate-current-line arg))))

(defun hpl/duplicate-region (num &optional start end)
  "Duplicates the region bounded by START and END NUM times.
If not START and END are provided, the current region-beginning and
region-end are used. Adds the duplicated text to the kill ring."
  (interactive "p")
  (let* ((start (or start (region-beginning)))
         (end (or end (region-end)))
         (region (buffer-substring start end)))
    (kill-ring-save start end)
    (goto-char start)
    (dotimes (i num)
      (insert region))))

(defun hpl/duplicate-current-line (num)
  "Duplicates the current line NUM times."
  (interactive "p")
  (when (eq (point-at-eol) (point-max))
    (goto-char (point-max))
    (newline)
    (forward-char -1))
  (hpl/duplicate-region num (point-at-bol) (1+ (point-at-eol))))

;; bind keys from table
;; Duplication:1 ends here

;; [[file:README.org::*New lines][New lines:1]]
(defun hpl/open-line-below ()
  "Opens a line below the point."
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun hpl/open-line-above ()
  "Open a line above the point."
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

;; bind keys from table
;; New lines:1 ends here

;; [[file:README.org::*Quoting regions][Quoting regions:1]]
(defun hpl/current-quotes-char ()
  "Gets the current quotes character."
  (nth 3 (syntax-ppss)))

(defalias 'point-is-in-string-p 'hpl/current-quotes-char)

(defun hpl/move-point-forward-out-of-string ()
  (while (point-is-in-string-p) (forward-char)))

(defun hpl/move-point-backward-out-of-string ()
  (while (point-is-in-string-p) (backward-char)))

(defun hpl/alternate-quotes-char ()
  "Gets the alternate quotes character."
  (if (eq ?' (hpl/current-quotes-char)) ?\" ?'))

(defun hpl/toggle-quotes ()
  (interactive)
  (if (point-is-in-string-p)
      (let ((old-quotes (char-to-string (hpl/current-quotes-char)))
            (new-quotes (char-to-string (hpl/alternate-quotes-char)))
            (start (make-marker))
            (end (make-marker)))
        (save-excursion
          (hpl/move-point-forward-out-of-string)
          (backward-delete-char 1)
          (set-marker end (point))
          (insert new-quotes)
          (hpl/move-point-backward-out-of-string)
          (delete-char 1)
          (insert new-quotes)
          (set-marker start (point))
          (replace-string new-quotes (concat "\\" new-quotes) nil start end)
          (replace-string (concat "\\" old-quotes) old-quotes nil start end)))
    (error "Point isn't in a string")))

;; bind-keys-from-table
;; Quoting regions:1 ends here

;; [[file:README.org::*Whitespace][Whitespace:1]]
(defun hpl/indent-buffer ()
  "Indent the current buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun hpl/cleanup-whitespace ()
  "Replace tabs and indent buffer."
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (while (re-search-forward "\t" nil t)
      (replace-match "        "))
    (hpl/indent-buffer)))

(bind-key "C-c w" 'hpl/cleanup-whitespace)
;; Whitespace:1 ends here

;; [[file:README.org::*Lisp directories (aka, `vendor')][Lisp directories (aka, `vendor'):1]]
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
;; Lisp directories (aka, `vendor'):1 ends here

;; [[file:README.org::*Exec Path][Exec Path:1]]
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

;; [[file:README.org::*Performance tuning][Performance tuning:1]]
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

;; [[file:README.org::*Frame Hooks][Frame Hooks:1]]
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
;; Frame Hooks:1 ends here

;; [[file:README.org::*XTerm][XTerm:1]]
;;; --- Integrate with terminals such as xterm

(global-set-key [mouse-4] (lambda () (interactive) (scroll-down 1)))
(global-set-key [mouse-5] (lambda () (interactive) (scroll-up 1)))

(autoload 'mwheel-install "mwheel")

(defun sanityinc/console-frame-setup ()
  (xterm-mouse-mode 1)
  (mwheel-install))


(add-hook 'after-make-console-frame-hooks 'sanityinc/console-frame-setup)
;; XTerm:1 ends here

;; [[file:README.org::*FontMs][FontMs:1]]
;;; --- Fonts
(if *is-macos*
    (progn
      (set-face-attribute 'default nil :font "JetBrainsMonoNL Nerd Font")
      (set-frame-font "JetBrainsMonoNL Nerd Font" nil t))
  (progn
    (set-face-attribute 'default nil :font "JetBrains Mono NL Nerd Font")
    (set-frame-font "JetBrains Mono NL Nerd Font" nil t)))


;;(set-face-attribute 'default nil
;;      :font "JetBrains Mono NL Nerd Font"
;;      :weight 'extrabold
;;      :height 110)

;;(set-face-attribute 'fixed-pitch nil
;;      :font "JetBrains Mono Nerd Font"
;;      :weight 'bold
;;      :height 110)
;; Fonts:1 ends here

;; [[file:README.org::*Ligatures][Ligatures:1]]
(use-package fira-code-mode
  :straight t
  :demand t
  :if (display-graphic-p)
  :hook prog-mode
  :custom (fira-code-mode-disabled-ligatures '("[]" ":" "x"))
  :config (fira-code-mode-set-font))
;; Ligatures:1 ends here

;; [[file:README.org::*Emoji][Emoji:1]]
;; Default Windows emoji font
(when (member "Segoe UI Emoji" (font-family-list))
  (set-fontset-font t 'symbol (font-spec :family "Segoe UI Emoji") nil 'prepend)
  (set-fontset-font "fontset-default" '(#xFE00 . #xFE0F) "Segoe UI Emoji"))

(when (member "Noto Color Emoji" (font-family-list))
  (set-fontset-font t 'symbol (font-spec :family "Noto Color Emoji") nil 'prepend)
  (set-fontset-font "fontset-default" '(#xFE00 . #xFE0F) "Noto Color Emoji"))

(when (member "Apple Color Emoji" (font-family-list))
  (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend)
  (set-fontset-font "fontset-default" '(#xFF00 . #xFE0F) "Apple Color Emoji"))
;; Emoji:1 ends here

;; [[file:README.org::*Icons][Icons:1]]
;;; --- Icons

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
;; Icons:1 ends here

;; [[file:README.org::*UI Elements][UI Elements:1]]
;;; --- Line Numbers

(setq column-number-mode t)
(setq line-number-mode t)
(global-display-line-numbers-mode t)

;; Defaults to 70. 80 is better.
(setq fill-column 80)

;; Sentences do not need double spaces to end.
(set-default 'sentence-end-double-space nil)

;; disable line numbers where they don't make sense or arent useful.
(dolist (mode '(org-mode-hook
                term-mode-hook
                treemacs-mode-hook
                dired-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode nil))))
;; UI Elements:1 ends here

;; [[file:README.org::*Themes][Themes:1]]
;;; --- Theming

(use-package monokai-pro-theme
  :ensure t
  :init
  (load-theme 'monokai-pro t))
;; Themes:1 ends here

;; [[file:README.org::*Themes][Themes:2]]
(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

(use-package highlight-escape-sequences
  :hook (prog-mode . hes-mode))
;; Themes:2 ends here

;; [[file:README.org::*GUI Frames][GUI Frames:1]]
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

(defun hpl/transparency (value &optional frame)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0-100 opaque:")
  (let ((frame (or frame (selected-frame))))
    (set-frame-parameter frame 'alpha value)))

(defun hpl/set-frame-transparency (&optional frame)
  "Sets the transparency on the selected frame."
  (hpl/transparency 98 frame))

(add-hook 'after-make-frame-functions 'hpl/set-frame-transparency)
(add-hook 'after-init-hook 'hpl/set-frame-transparency)
;; GUI Frames:1 ends here

;; [[file:README.org::*macOS Keys][macOS Keys:1]]
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
;; macOS Keys:1 ends here

;; [[file:README.org::*General keys][General keys:1]]
;;; General keybinds-

(defun hplogsdon/kill-this-buffer ()
  (interactive)
  (catch 'quit
    (save-window-excursion
      (let (done)
        (when (and buffer-file-name (buffer-modified-p))
          (while (not done)
            (let ((response (read-char-choice
                             (format "Save file %s? (y, n, d, q) " (buffer-file-name))
                             '(?y ?n ?d ?q))))
              (setq done (cond
                          ((eq response ?q) (throw 'quit nil))
                          ((eq response ?y) (save-buffer) t)
                          ((eq response ?n) (set-buffer-modified-p nil) t)
                          ((eq response ?d) (diff-buffer-with-file) nil))))))
        (kill-buffer (current-buffer))))))

(global-set-key [s-tab] 'next-buffer)
(global-set-key [S-s-iso-lefttab] 'previous-buffer)
(global-set-key ["M-{"] 'next-buffer)
(global-set-key ["M-}"] 'previous-buffer)

;; change window
(global-set-key [(C-tab)] 'other-window)
(global-set-key [(C-M-tab)] 'other-window)

;; Remap kill buffer to this, which asks to diff the buffer or close
(global-set-key [remap kill-buffer] 'hplogsdon/kill-this-buffer)
(global-set-key (kbd "C-x k") 'hplogsdon/kill-this-buffer)

;; Revert buffer
(global-set-key (kbd "C-<f5>") 'revert-buffer)

;; Jump to scratch
(global-set-key (kbd "C-<f2>") (lambda () (interactive) (switch-to-buffer "*scratch*")))

;; Go to line
(global-set-key (kbd "M-g") 'goto-line)
;; General keys:1 ends here

;; [[file:README.org::*Unset conveniences.][Unset conveniences.:1]]
(setq shift-select-mode nil)
;; Unset conveniences.:1 ends here

;; [[file:README.org::*Unset conveniences.][Unset conveniences.:2]]
;;  (global-unset-key [up])
;;  (global-unset-key [down])
;;  (global-unset-key [left])
;;  (global-unset-key [right])
;;  (global-unset-key [M-left])
;;  (global-unset-key [M-right])
;; Unset conveniences.:2 ends here

;; [[file:README.org::*Mouse / Trackpad][Mouse / Trackpad:1]]
(use-package mwheel
  :ensure nil ;; builtin
  :config (setq mouse-wheel-scroll-amount '(2 ((shift) . 1))
                mouse-wheel-progressive-speed nil))
;; Mouse / Trackpad:1 ends here

;; [[file:README.org::*File Management][File Management:1]]
(setq confirm-kill-processes nil
      create-lockfiles nil          ;; Dont create .# files
      make-backup-files nil)
;; File Management:1 ends here

;; [[file:README.org::*dired][dired:1]]
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
;; dired:1 ends here

;; [[file:README.org::*isearch][isearch:1]]
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
   ("C-c s"    . isearch-forward-symbol-at-point)
   ("C-c o"    . sanityinc/isearch-occur)
   ("C-c C-o"  . sanityinc/isearch-occur)
   :map isearch-mode-map
   ("M-<down>" . isearch-ring-advance)
   ("M-<up>"   . isearch-ring-retreat)
   :map minibuffer-local-isearch-map
   ("M-<down>" . next-history-element)
   ("M-<up>"    . previous-history-element))

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
;; isearch:1 ends here

;; [[file:README.org::*grep][grep:1]]
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
;; grep:1 ends here

;; [[file:README.org::*uniquify][uniquify:1]]
;;; --- Configure uniquification of buffer name

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'reverse
        uniquify-separator " • "
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re "^\\*"))
;; uniquify:1 ends here

;; [[file:README.org::*flymake][flymake:1]]
;;; --- Configure flymake global behavior

(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
              ("C-c l" . flymake-show-buffer-diagnostics)
              ("C-c n" . flymake-goto-next-error)
              ("C-c p" . flymake-goto-prev-error)
              ("C-c c" . flymake-start)))
;; flymake:1 ends here

;; [[file:README.org::*xref][xref:1]]
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
;; xref:1 ends here

;; [[file:README.org::*Language Server Protocol][Language Server Protocol:1]]
;;; --- LSP Support

(use-package lsp-mode
  :after (:all xref)
  :commands (lsp lsp-deferred)
  :hook ((prog-mode . lsp-deferred))

  :custom
  (lsp-enable-folding nil)
  (lsp-enable-links nil)
  (lsp-enable-snippet nil)
  (lsp-keymap-prefix "C-c ;")
  (lsp-session-file (expand-file-name "lsp-session-v1" emacs-user-directory))
  (read-process-output-max (* 1024 1024)))
;; Language Server Protocol:1 ends here

;; [[file:README.org::*Language Server Protocol][Language Server Protocol:2]]
(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom-face
  (lsp-ui-doc-background ((t (:background nil))))
  (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)
              ("C-c u" . lsp-ui-imenu))
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-border (face-foreground 'default))
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions nil))
;; Language Server Protocol:2 ends here

;; [[file:README.org::*Language Server Protocol][Language Server Protocol:3]]
(use-package dap-mode
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))
;; Language Server Protocol:3 ends here

;; [[file:README.org::*recentf][recentf:1]]
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
;; recentf:1 ends here

;; [[file:README.org::*minibuffer][minibuffer:1]]
;;;  ---

(defun switch-to-minibuffer ()
  "Switch to minibuffer."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))

(bind-key "M-m" 'switch-to-minibuffer)
;; minibuffer:1 ends here

;; [[file:README.org::*dabbrev][dabbrev:1]]
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind
  (("M-/" . dabbrev-completion)
   ("C-M-/" . dabbrev-expand))

  ;; Other useful Dabbrev configuration
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'"))
  (setq dabbrev-case-fold-search nil))
;; dabbrev:1 ends here

;; [[file:README.org::*hippie-expand][hippie-expand:1]]
;;; --- Settings for hippie-expand

(use-package hippie-exp
  :ensure nil
  :bind ([remap dabbrev-expand] . hippie-expand)
  :commands (hippie-expand)

  :init
  (defadvice hippie-expand (around hippie-expand-case-fold activate)
    "Try to do case-sensitive matching (not effective with all functions)."
    (let ((case-fold-search nil))
      ad-do-it))

  :config
  (setq hippie-expand-try-functions-list
        '(;; Try to expand word "dynamically", searching just the current buffer
          try-expand-dabbrev
          ;; Try to expand word "dynamically", searching ...?
          try-expand-dabbrev-visible
          ;; Try to expand word "dynamically", searching all other buffers.
          try-expand-dabbrev-all-buffers
          ;; Try to expand word "dynamically", searching the kill ring
          try-expand-dabbrev-from-kill
          ;; Try to complete text as a filename, as many characters are unique
          try-complete-file-name-partially
          ;; Try to complete text as a filename.
          try-complete-file-name
          ;; Try to complete before point according to all abbrev tables.
          try-expand-all-abbrevs
          ;; Try to complete the current line to a list in the buffer
          try-expand-list
          ;; Try to complete the current line to a line in the buffer
          try-expand-line
          ;; Try to complete the current line to an entire line in a different buffer.
          try-expand-line-all-buffers
          ;; Try to complete text using flyspell
          ;try-flyspell
          ;; Try to complete as an Emacs Lisp symbol, as many characters are unique.
          try-complete-lisp-symbol-partially
          ;; Try to complete word as an Emacs Lisp symbol.
          try-complete-lisp-symbol)))
;; hippie-expand:1 ends here

;; [[file:README.org::*corfu][corfu:1]]
;;; --- Interactive completion in buffers

;; Corfu is responsible for interactive completion
(use-package corfu
  :init
  (global-corfu-mode)

  :custom
  (corfu-auto nil)     ; Popup appears automatically
  (corfu-auto-delay 0.2)    ;
  (corfu-auto-prefix 3)
  (corfu-quit-no-match 'separator)
  (corfu-preview-current t))    ; Show candidate on pointer
;; corfu:1 ends here

;; [[file:README.org::*Cape][Cape:1]]
;;; --- Cape

;; Adds more completion source backends for Corfu
(use-package cape
  :bind
  (("C-c p" . cape-prefix-map)
   ("C-c M-p" . completion-at-point))

  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))


;; Adjust some of the emacs defaults if we load this module
(use-package emacs
  :ensure nil
  :custom
  ;; limit the height of the *Completions* buffer
  (completions-max-height 15)
  ;; Use TAB for completions first, then indent
  (tab-always-indent 'complete))
;; Cape:1 ends here

;; [[file:README.org::*windows][windows:1]]
;;; --- Working with windows within frames

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
   ("C-S-l" . #'buf-move-right)))
;; windows:1 ends here

;; [[file:README.org::*sessions][sessions:1]]
;;; --- Save and restore editor sessions between restarts

;; save a list of open files in ~/.emacs.d/.emacs.desktop
(setq desktop-path (list user-emacs-directory)
      desktop-auto-save-timeout 600)
(desktop-save-mode 1)

(defun sanityinc/desktop-time-restore (orig &rest args)
  (let ((start-time (current-time)))
    (prog1
        (apply orig args)
      (message "Desktop restored in %.2fms"
               (sanityinc/time-subtract-millis (current-time)
                                               start-time)))))
(advice-add 'desktop-read :around 'sanityinc/desktop-time-restore)

(defun sanityinc/desktop-time-buffer-create (orig ver filename &rest args)
  (let ((start-time (current-time)))
    (prog1
        (apply orig ver filename args)
      (message "Desktop: %.2fms to restore %s"
               (sanityinc/time-subtract-millis (current-time)
                                               start-time)
               (when filename
                 (abbreviate-file-name filename))))))
(advice-add 'desktop-create-buffer :around 'sanityinc/desktop-time-buffer-create)

;; Restore histories and registers after saving
(setq-default history-length 1000)
(add-hook 'after-init-hook 'savehist-mode)

(use-package session
  :config
  (setq session-save-file (locate-user-emacs-file ".session"))
  (setq session-name-disable-regexp "\\(?:\\`'/tmp\\|\\.git/[A-Z_]+\\'\\)")
  (setq session-save-file-coding-system 'utf-8)

  (add-hook 'after-init-hook 'session-initialize)

  (setq desktop-globals-to-save
        '((compile-history      . 30)
          (dired-regexp-history . 20)
          (file-name-history    . 100)
          (grep-find-history    . 30)
          (grep-history         . 30)
          (minibuffer-history   . 50)
          (org-tags-history     . 50)
          (regexp-history       . 60)
          (regexp-search-ring   . 20)
          (search-ring          . 20))))
;; sessions:1 ends here

;;; --- Multiple Major Modes support

(use-package mmm-mode
  :ensure t
  :defer t
  :config
  (setq mmm-global-mode 'buffers-with-submode-classes)
  (setq mmm-submode-mode-line-format "~M > [~m]"
        mmm-primary-mode-display-name t
        mmm-buffer-mode-display-name t)
  (setq mmm-submode-decoration-level 3)

  ;;(mmm-add-mode-ext-classes 'html-mode "\\.php\\'" 'html-php)

  (unless (boundp 'editing-prefix)
    (define-prefix-command 'editing-prefix))
  (define-key editing-prefix (kbd "m") 'mmm-mode) ;; enable mmm on region

  ;; Submode classes
  ;;(mmm-add-classes
  ;; '((embedded-css
  ;;   :submode css
  ;;   :face mmm-declaration-submode-face
  ;;    :front "<style[^>]&>"
  ;;   :back "</style>")))

  ;; Submode groups
  ;;(mmm-add-to-group 'html-js '((js-html
  ;;                              :submode javascript
  ;;                              :face mmm-code-submode-face
  ;;                              :front "%=%"
  ;;                              :back "%=%"
  ;;                              :end-not-begin t)))
  )

;; [[file:README.org::*editing-utils][editing-utils:1]]
;;; --- Day-to-Day editing helpers

;; Advises kill-region "C-w" so that if no region is selected, it kills/copies the current line.
(advice-add 'kill-region :before
            (lambda (&rest args)
              "When called interactively with no active region, kill a single line instead."
              (when (called-interactively-p 'interactive)
                (unless mark-active
                  (setq args (list (line-beginning-position)
                                   (line-beginning-position 2)))))))
;; editing-utils:1 ends here

;; [[file:README.org::*whitespace][whitespace:1]]
;;; --- Special handling for whitespace

;; Show whitespace issues
(use-package whitespace
  :diminish (global-whitespace-mode
             whitespace-mode
             whitespace-newline-mode)
  :bind
  ("C-c T w" . whitespace-mode)
  :hook
  (prog-mode . whitespace-mode)

  :config
  (setq whitespace-line-column 120
        whitespace-style '(face
                           tabs
                           indent
                           tab-mark
                           empty
                           trailing
                           lines-tail)))

(use-package whitespace-cleanup-mode
  :ensure t
  :diminish whitespace-cleanup-mode

  :bind (("C-c T W" . whitespace-cleanup-mode)
         ("C-c e w" . whitespace-cleanup))

  :hook
  (prog-mode . whitespace-cleanup-mode)

  :config
  (global-whitespace-cleanup-mode 1))

(global-set-key [remap just-one-space] 'cycle-spacing)
;; whitespace:1 ends here

;; [[file:README.org::*Version Control][Version Control:1]]
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
;; Version Control:1 ends here

;; [[file:README.org::*git][git:1]]
;;; --- Git SCM Support

(use-package git-gutter
  :ensure t
  :defer t
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 1))


(use-package git-gutter-fringe
  :ensure t
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [244] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [244] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 244 240] nil nil 'bottom))


(use-package git-modes
  :mode
  ("\\.?gitignore\\'"
   "\\.?gitignore_global\\'"
   "\\.?dockerignore\\'")
  :defer t)


(use-package git-timemachine
  :bind
  (("C-x v t" . git-timemachine-toggle))
  :hook
  (git-timemachine-mode . display-line-numbers-mode))


(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         :map magit-status-mode-map
         ("C-x C-k" . hpl/magit-kill-file-on-line)
         ("q" . hpl/magit-quit-session)
         ("W" . hpl/magit-toggle-whitespace))

  :config
  (progn
    (setq magit-auto-revert-mode nil)
    (setq magit-repository-directories '("~/Projects/"))

                                        ;(defadvice magit-status (around magit-fullscreen activate)
                                        ;  (unless (get-register :magit-fullscreen)
                                        ;    (window-configuration-to-register :magit-fullscreen))
                                        ;  ad-to-it
                                        ;  (delete-other-windows))

    (defun hpl/magit-kill-file-on-line ()
      ""
      (interactive)
      (magit-visit-item)
      (delete-current-buffer-file)
      (magit-refresh))

    (defun hpl/magit-quit-session ()
      "Restores the previous window configuration"
      (interactive)
      (kill-buffer)
      (jump-to-register :magit-fullscreen)
      (set-register :magit-fullscreen nil))

    (defun hpl/magit-toggle-whitespace ()
      ""
      (interactive)
      (if (member "-w" magit-diff-options)
          (hpl/magit-dont-ignore-whitespace)
        (hpl/magit-ignore-whitespace)))

    (defun hpl/magit-dont-ignore-whitespace ()
      ""
      (interactive)
      (add-to-list 'magit-diff-options "-w")
      (magit-refresh))

    (defun hpl/magit-ignore-whitespace ()
      ""
      (interactive)
      (setq magit-diff-options (remove "-w" magit-diff-options))
      (magit-refresh))))
;; git:1 ends here

;; [[file:README.org::*github][github:1]]
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
;; github:1 ends here

;; [[file:README.org::*gitlab][gitlab:1]]
;;; --- Gitlab Integration
;; gitlab:1 ends here

;; [[file:README.org::*ibuffer][ibuffer:1]]
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
  (use-package ibuffer-vc
    :commands (ibuffer-vc-set-filter-groups-by-vc-root)
    :custom
    (ibuffer-vc-skip-if-remote 'nil))

  (setq ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold)))
  (setq ibuffer-formats '((mark modified read-only locked
                                " " (name 35 35 :left :elide)
                                " " (size 9 -1 :right)
                                " " (mode 16 16 :left :elide)
                                " " filename-and-process)
                          (mark modified read-only vc-status-mini
                                " " (name 22 22 :left :elide)
                                " " (size 9 -1 :right)
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
;; ibuffer:1 ends here

;; [[file:README.org::*projectile][projectile:1]]
;;; --- Projectile Project configuration
(use-package projectile
  :ensure projectile
  :diminish projectile-mode

  :bind
  (("C-c p" . projectile-switch-project))

  :init
  (setq-default
   projectile-cache-file (expand-file-name ".projectile-cache" user-emacs-directory)
   projectile-keymap-prefix (kbd "C-c C-p")
   projectile-known-projects-file (expand-file-name ".projectile-bookmarks" user-emacs-directory))

  :config
  (projectile-global-mode 1)
  (setq projectile-enable-caching t
        projectile-create-missing-test-files t
        ;;projectile-completion-system 'ivy
        projectile-mode-line-prefix " Proj"
        projectile-mode-line '(:eval (projectile-project-name))
        projectile-use-git-grep t
        projectile-commander-methods nil))


(use-package ibuffer-projectile
  :after (projectile)
  :bind
  (:map ibuffer-mode-map
        ("c" . clean-buffer-list)
        ("n" . ibuffer-forward-filter-group)
        ("p" . ibuffer-backwards-filter-group))
  :hook
  ((ibuffer . (lambda ()
                (ibuffer-projectile-set-filter-groups)
                (unless (eq ibuffer-sorting-mode 'alphabetic)
                  (ibuffer-do-sort-by-alphabetic))))))
;; projectile:1 ends here

;; [[file:README.org::*markdown][markdown:1]]
(use-package markdown-mode
  :mode ("\\.\\(md\\|markdown\\)\\'"))
;; markdown:1 ends here

;; [[file:README.org::*csv][csv:1]]
(use-package csv-mode
  :mode ("\\.\\(csv\\|tsv\\)\\'"))
;; csv:1 ends here

;; [[file:README.org::*javascript][javascript:1]]

;; javascript:1 ends here

;; [[file:README.org::*orgmode][orgmode:1]]
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
  (org-directory "~/OneDrive/org/")
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
  (org-ellipses "  ")

  :custom-face
  (org-agenda-current-time ((t (:foreground "spring green"))))

  :hook ((org-mode . (lambda ()
                       (set-face-attribute 'org-level-1 nil :height 1.4)
                       (set-face-attribute 'org-level-2 nil :height 1.3)
                       (set-face-attribute 'org-level-3 nil :height 1.2)
                       (set-face-attribute 'org-level-4 nil :height 1.1)
                       (set-face-attribute 'org-level-5 nil :height 1.1)))
         (org-mode . (lambda ()
                       (push '("TODO"      . ?▲) prettify-symbols-alist)
                       (push '("NEXT"      . ?➜) prettify-symbols-alist)
                       (push '("DONE"      . ?✓) prettify-symbols-alist)
                       (push '("CANCELLED" . ?✘) prettify-symbols-alist))))

  :config
  (unless (version<= org-version "9.2")
    (require 'org-tempo))

  (when (or (file-directory-p "~/OneDrive/org/agenda.org") (file-directory-p "~/OneDrive/org/journal"))
    (setq org-agenda-files (list "~/OneDrive/org/agenda.org" "~/OneDrive/org/journal"))))

(use-package org-journal
  :bind (("C-c j n" . org-journal-new-entry)
         ("C-c j t" . hpl/org-journal-today))

  :custom
  (org-journal-date-prefix "#+TITLE: ")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-dir "~/OneDrive/org/journal")
  (org-journal-date-format "%Y-%m-%d")

  :config
  (defun hpl/org-journal-today ()
    (interactive)
    (org-journal-new-entry t)))
;; orgmode:1 ends here

;; [[file:README.org::*Java][Java:1]]
(use-package lsp-java
  :hook (java-mode . lsp-deferred))
;; Java:1 ends here

;; [[file:README.org::*nxml][nxml:1]]
;;; --- XML

(use-package nxml-mode
  :straight nil ;; builtin
  :hook (nxml-mode . lsp-deferred)
  :mode ("\\.\\(xml\\|xsd\\|wsdl\\)\\'")
  )
;; nxml:1 ends here

;; [[file:README.org::*docker][docker:1]]
(use-package dockerfile-mode
  :mode "Dockerfile\\'")
;; docker:1 ends here

;; [[file:README.org::*ini Files][ini Files:1]]
(use-package ini-mode
  :mode "\\.ini\\'")
;; ini Files:1 ends here

;; [[file:README.org::*Rainbow Parens][Rainbow Parens:1]]
(show-paren-mode 1)

(use-package rainbow-delimiters
  :straight t

  :hook
  ((prog-mode . rainbow-delimiters-mode))

  :config
  ;; set colors to travel through the visual spectrum from red to blue
  '(rainbow-delimiters-depth-1-face ((t (:foreground "light slate blue"))))
  '(rainbow-delimiters-depth-2-face ((t (:foreground "cyan"))))
  '(rainbow-delimiters-depth-3-face ((t (:foreground "lime green"))))
  '(rainbow-delimiters-depth-4-face ((t (:foreground "yellow green"))))
  '(rainbow-delimiters-depth-5-face ((t (:foreground "yellow"))))
  '(rainbow-delimiters-depth-6-face ((t (:foreground "goldenrod"))))
  '(rainbow-delimiters-depth-7-face ((t (:foreground "dark orange"))))
  '(rainbow-delimiters-depth-8-face ((t (:foreground "orange red"))))
  '(rainbow-delimiters-depth-9-face ((t (:foreground "red2")))))
;; Rainbow Parens:1 ends here

;; [[file:README.org::*Color Strings][Color Strings:1]]
(use-package rainbow-mode
  :straight t
  :diminish rainbow-mode "🌈"
  :hook
  ((prog-mode . rainbow-mode)))
;; Color Strings:1 ends here

;; [[file:README.org::*paredit][paredit:1]]
;;; --- Paredit mode

(use-package paredit
  :ensure t
  :diminish (paredit-mode))
;;  :hook ((lisp-mode             . paredit-mode)
;;  (cider-mode            . paredit-mode)
;;  (cider-repl-mode       . paredit-mode)
;;     (clojure-mode          . paredit-mode)
;;  (emacs-lisp-mode       . paredit-mode))
;; paredit:1 ends here

;; [[file:README.org::*lisp][lisp:1]]
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
  :diminish "λ"
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
;; lisp:1 ends here

;; [[file:README.org::*Clojure Mode][Clojure Mode:1]]
;;; --- Clojure lanaguage
(use-package clojure-mode
  :mode (("\\.boot$"  . clojure-mode)
         ("\\.clj$"   . clojure-mode)
         ("\\.cljc$"  . clojure-mode)
         ("\\.cljs$"  . clojurescript-mode)
         ("\\.edn$"   . clojure-mode)
         ("lein-env$" . clojure-mode))
  :config
  (use-package align-cljlet
    :bind (:map clojure-mode-map
                ("C-! a a" . align-cljlet)
                :map clojurescript-mode-map
                ("C-! a a" . align-cljlet)
                :map clojurec-mode-map
                ("C-! a a" . align-cljlet))))
;; Clojure Mode:1 ends here

;; [[file:README.org::*Clojure Refactor][Clojure Refactor:1]]
(use-package clj-refactor
  :disabled
  :init
  (defun hpl/clj-refactor-mode-hook ()
    ;;(yas-minor-mode 1)
    (clj-refactor-mode 1))
  (add-hook 'clojure-mode-hook #'hpl/clj-refactor-mode-hook)
  (setq cljr-clojure-test-declaration "[clojure.test :refer :all]"
        cljr-cljs-clojure-test-declaration "[cljs.test :refer-macros [deftest is use-fixtures]]")
  :config
  (cljr-add-keybindings-with-prefix "<menu>")
  (add-to-list 'cljr-magic-require-namespaces
               '("s" . "clojure.spec.alpha")))
;; Clojure Refactor:1 ends here

;; [[file:README.org::*CIDER][CIDER:1]]
(use-package cider
  :bind (:map cider-repl-mode-map
              ("M-r" . cider-refresh)
              ("C-c r" . cider-repl-reset)
              ("M-R" . hpl/cider-user-repl-tools)
              :map clojure-mode-map
              ("C-M-r" . hpl/cider-refresh)
              ("C-c C-v" . hpl/cider-start-http-server)
              ("C-c u" . hpl/cider-user-ns)
              :map cider-mode-map
              ("C-c u" . hpl/cider-user-ns))
  
  :hook
  (;; Enable paredit in the cider REPL
   ;;(cider-mode . paredit-mode)
   ;; Provides minibuffer docs when writing to the REPL
   (cider-mode . cider-turn-on-eldoc-mode))
  
  :config
  (setq nrepl-hide-special-buffers t
        nrepl-popup-stacktraces-in-repl t
        nrepl-history-file "~/.emacs.d/nrepl-history"

        cider-mode-line " CIDER"
        cider-repl-display-in-current-window t
        ;; Where to store cider history
        cider-repl-history-file "~/.emacs.d/.cider-repl-history"
        ;; Go right to the REPL when it connects
        cider-repl-pop-to-buffer-on-connect t
        ;; When there is a cider error, show its buffer and switch to it
        cider-show-error-buffer nil
        cider-auto-select-error-buffer nil
        ;; wrap when navigating history
        cider-repl-wrap-history t
        
        cider-repl-use-pretty-printing t
        cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")

  (defun hpl/cider-use-repl-tools ()
    (interactive)
    (cider-interactive-repl
     "(use 'clojure.repl)"))

  (defun hpl/cider-start-http-server ()
    (interactive)
    (cider-load-current-buffer)
    (let ((ns (cider-current-ns)))
      (cider-repl-set-ns ns)
      (cider-interactive-eval (format "(println '(def server (%s/start))) (println 'server)" ns))
      (cider-interactive-eval (format "(def server (%s/start)) (println server)" ns))))

  (defun hpl/cider-refresh ()
    (interactive)
    (cider-interactive-eval "(user/reset)"))

  (defun hpl/cider-user-ns ()
    (interactive)
    (cider-repl-set-ns "user"))

  (fset 'cider-eval-last-sexp-and-comment
        "\C-u\C-x\C-e\C-a\260 ;; \C-e")

  (bind-key "C-j" 'cider-eval-last-sexp-and-comment clojure-mode-map)

  ;; this snippet comes from schmir https://github.com/schmir/.emacs.d/blob/master/lisp/setup-clojure.el
  (advice-add 'cider-load-buffer :after
              (lambda (&rest _)
                "Switch to namespace"
                (cider-repl-set-ns (cider-current-ns))
                (cider-switch-to-repl-buffer)))

  ;; Fix cond indenting
  (put 'cond 'clojure-backtracking-indent '(2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4)))
;; CIDER:1 ends here

;; [[file:README.org::*which-key][which-key:1]]
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

;; [[file:README.org::*Dashboard][Dashboard:1]]
(use-package dashboard
  :config
  (setq dashboard-startup-banner 'logo
        dashboard-banner-logo-title "EMACS!"
        dashboard-items nil
        dashboard-set-footer nil))
;; Dashboard:1 ends here

;;; --- Terminals in Emacs

;;(use-package vterm
;;  :ensure t)

;;(use-package eat
;;  :ensure t)

;;; --- Anthropic Claude Code
(use-package claude-code
  :ensure t
  :after (:any eat vterm)
  :straight
  (claude-code :type git :host github :repo "stevemolitor/claude-code.el")

  :config
  (claude-code-mode)
  ;; eat is the default, but we could change to vterm
  ;;(setq claude-code-terminal-backend 'vterm)
  ;;(setq claude-code-terminal-backend 'eat)

  :bind-keymap
  ("C-c C" . claude-code-command-mode))

;; [[file:README.org::*eldoc][eldoc:1]]
(use-package eldoc
  :ensure nil ;; builtin
  ;;:diminish eldoc-mode
  :config
  (setq eldoc-idle-delay 0.4))
;; eldoc:1 ends here

;; [[file:README.org::*Footer][Footer:1]]
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


;;; init.el ends here
;; Footer:1 ends here

;; Local Variables:
;; coding: utf-8-unix
;; no-byte-compile: t
;; End:
(provide 'init)
