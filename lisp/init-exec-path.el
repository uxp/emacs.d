;;; init-exec-path.el --- Setup exec-path to help Emacs find packages -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package exec-path-from-shell)

(when (or (memq window-system '(mac ns x pgtk))
          (unless (memq system-type '(ms-dos windows-nt))
            (daemonp)))
  (use-package exec-package-with-shell
    :ensure t
    :config
    (exec-path-from-shell-initialize)
    (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH"))
      (add-to-list 'exec-path-from-shell-variables var))))

(provide 'init-exec-path)
;;; init-exec-path.el ends here
