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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (multi-term origami elfeed-org notmuch editorconfig smartparens change-inner yasnippet ob-http org-ref expand-region link-hint edit-indirect multiple-cursors json-mode nix-mode direnv flycheck-rust rust-mode dap-mode lsp-treemacs lsp-ivy lsp-ui lsp-mode which-key rainbow-delimiters magit company-flx company-posframe company flycheck counsel one-themes use-package exwm)))
 '(sp-base-key-bindings (quote sp)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
