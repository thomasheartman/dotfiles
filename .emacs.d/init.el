;;; heartmacs --- Summary

;;; Commentary:
;; This file outsources the Emacs configuration to a file called
;; `config.org' in the same directory.

;;; Code:
(eval-when-compile (defvar my-config-dir))
(unless (boundp 'my-config-dir)
  (setq my-config-dir (file-name-directory (or load-file-name buffer-file-name))))


(message "Using config dir: %s" my-config-dir)

(org-babel-load-file (expand-file-name "config.org" my-config-dir))

(provide 'init)
;;; init.el ends here
