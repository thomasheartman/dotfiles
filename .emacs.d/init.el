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

(setq straight-use-package-by-default t
      straight-fix-flycheck t)

;; taken from https://github.com/radian-software/straight.el/issues/1059
(define-obsolete-variable-alias
  'native-comp-deferred-compilation-deny-list
  'native-comp-jit-compilation-deny-list
  "Renamed in emacs#95692f6")

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
  ;; catch emacs updates that have native compiled leftovers
  (unless (catch 'emacs-version-changed
            (load bootstrap-file nil 'nomessage))
    (when (boundp 'comp-eln-load-path)
      ;; remove leftovers
      (when (y-or-n-p (format "Delete '%s'? " (car comp-eln-load-path)))
        (delete-directory (file-truename (expand-file-name (car comp-eln-load-path))) t))
      ;; try loading again
      (load bootstrap-file nil 'nomessage))))
(message "Configured straight.el")

(straight-use-package 'use-package)

(org-babel-load-file (expand-file-name "config.org" my-config-dir))

(provide 'init)
;;; init.el ends here
