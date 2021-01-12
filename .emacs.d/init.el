;;; heartmacs --- Summary

;;; Commentary:
;; This file outsources the Emacs configuration to a file called
;; `config.org' in the same directory.

;;; Code:
(eval-when-compile (defvar my-config-dir))
(unless (boundp 'my-config-dir)
  (setq my-config-dir (file-name-directory (or load-file-name buffer-file-name))))


(message "Using config dir: %s" my-config-dir)

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

(straight-use-package 'use-package)

(use-package git)
(use-package org
  :init

  ;; The following is a temporary hack until straight.el supports
  ;; building Org, see:
  ;;
  ;; * https://github.com/raxod502/straight.el/issues/211
  ;; * https://github.com/raxod502/radian/issues/410
  ;;
  ;; There are three things missing from our version of Org: the
  ;; functions `org-git-version' and `org-release', and the feature
  ;; `org-version'. We provide all three of those ourself, therefore.

  (defun org-git-version ()
    "The Git version of org-mode.
  Inserted by installing org-mode or when a release is made."
    (require 'git)
    (let ((git-repo (expand-file-name
                     "straight/repos/org/" user-emacs-directory)))
      (string-trim
       (git-run "describe"
                "--match=release\*"
                "--abbrev=6"
                "HEAD"))))

  (defun org-release ()
    "The release version of org-mode.
  Inserted by installing org-mode or when a release is made."
    (require 'git)
    (let ((git-repo (expand-file-name
                     "straight/repos/org/" user-emacs-directory)))
      (string-trim
       (string-remove-prefix
        "release_"
        (git-run "describe"
                 "--match=release\*"
                 "--abbrev=0"
                 "HEAD")))))

  (provide 'org-version))


(message "Configured straight.el")

(org-babel-load-file (expand-file-name "config.org" my-config-dir))

(provide 'init)
;;; init.el ends here
