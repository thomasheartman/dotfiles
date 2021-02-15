;;; heartmacs --- Summary  -*- lexical-binding: t; -*-

;;; Commentary:
;; This file outsources the Emacs configuration to a file called
;; `config.org' in the same directory.

;;; Code:

;; always follow symlinks
;; this is particularly relevant if you symlink your repo.
(setq vc-follow-symlinks t)

(eval-when-compile (defvar my-config-dir))
(let ((config-dir (file-name-directory (file-truename (or load-file-name (buffer-file-name))))))
  (setq user-emacs-directory config-dir)
  (unless (boundp 'my-config-dir)
    (setq my-config-dir config-dir)))


(message "Using config dir %s and user-emacs-directory %s" my-config-dir user-emacs-directory)

(when (not (version<= emacs-version "28"))
  (load-file (expand-file-name "my-obsolete-fns.el" my-config-dir)))

(setq straight-use-package-by-default t
      straight-fix-flycheck t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(message "Configured straight.el")

(straight-use-package 'use-package)

(org-babel-load-file (expand-file-name "config.org" my-config-dir))

(provide 'init)
;;; init.el ends here
