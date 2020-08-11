;; Package archives
;;    Set up archives for packages:

;; [[file:~/.emacs.d_new/config.org::*Package archives][Package archives:1]]
(setq package-archives '(("gnu" . "https://elpa.gnu.org./packages/")
			 ("melpa" . "https://melpa.org/packages/")))
;; Package archives:1 ends here

;; use-package
;;    Before we do anything else, let's make sure we've got [[https://github.com/jwiegley/use-package][use-package]]
;;    ready to go! This bit of setup is a modified version of what Greg
;;    Stein's example from Caches to Caches post [[http://cachestocaches.com/2015/8/getting-started-use-package/][Getting started with
;;    use-package]].

;; [[file:~/.emacs.d_new/config.org::*use-package][use-package:1]]
(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t))
;; use-package:1 ends here

;; Frames
;;     Get rid of tool bars and menu bars:

;; [[file:~/.emacs.d_new/config.org::*Frames][Frames:1]]
(menu-bar-mode -1)
(tool-bar-mode -1)
;; Frames:1 ends here

;; Windows
;;     Let's get rid of scroll bars:

;; [[file:~/.emacs.d_new/config.org::*Windows][Windows:1]]
(set-scroll-bar-mode nil)
;; Windows:1 ends here

;; Theme

;; [[file:~/.emacs.d_new/config.org::*Theme][Theme:1]]
(use-package one-themes
	     :config
	     (load-theme 'one-dark t))
;; Theme:1 ends here

;; Font
;;     The default font size is a bit small on my monitors, so we'll
;;     increase it sufficiently.

;; [[file:~/.emacs.d_new/config.org::*Font][Font:1]]
(set-face-attribute 'default nil :height 200)
;; Font:1 ends here

;; ido mode
;;     Inspired by the [[https://www.masteringemacs.org/article/introduction-to-ido-mode][Introduction to ido mode article]] from Mastering
;;     Emacs, let's enable ido mode everywhere and also enable
;;     flex-matching.

;; [[file:~/.emacs.d_new/config.org::*ido mode][ido mode:1]]
(setq ido-enable-flex-matching t
      ido-everywhere t)
(ido-mode 1)
;; ido mode:1 ends here



;; We can also use ido-mode for find file at point.

;; [[file:~/.emacs.d_new/config.org::*ido mode][ido mode:2]]
(setq ido-use-filename-at-point 'guess)
;; ido mode:2 ends here



;; When entering a buffer name that doesn't exist when changing
;; buffers, let's let ido-mode create a new buffer.

;; [[file:~/.emacs.d_new/config.org::*ido mode][ido mode:3]]
(setq ido-create-new-buffer 'always)
;; ido mode:3 ends here

;; Prompts
;;     Taking another cue from Mastering Emacs ([[https://www.masteringemacs.org/article/disabling-prompts-emacs][Disabling Prompts in
;;     Emacs]]), let's reduce the amount of prompts we come across in
;;     Emacs:

;; [[file:~/.emacs.d_new/config.org::*Prompts][Prompts:1]]
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil)
;; Prompts:1 ends here



;; We can also disable prompts about killing buffers with live
;; processes attached.

;; [[file:~/.emacs.d_new/config.org::*Prompts][Prompts:2]]
(setq kill-buffer-query-funcions
      (remq 'process-kill-buffer-query-function
	    kill-buffer-query-functions))
;; Prompts:2 ends here

;; Startup
;;     Let's get rid of splash screens and startup messages that we don't
;;     need (also from [[https://www.masteringemacs.org/article/disabling-prompts-emacs][Disabling Prompts in Emacs]]):

;; [[file:~/.emacs.d_new/config.org::*Startup][Startup:1]]
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)
;; Startup:1 ends here

;; recent files (recentf)                                       :keybinding:
;;     Mickey Petersen suggests using the recent files package in his
;;     [[https://www.masteringemacs.org/article/find-files-faster-recent-files-package][Find Files Faster with the Recent Files Package]], so we'll copy his
;;     suggested configuration here.


;; [[file:~/.emacs.d_new/config.org::*recent files (recentf)][recent files (recentf):1]]
(require 'recentf)

(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

(recentf-mode t)

(setq recentf-max-saved-items 50)

(defun ido-recentf-open ()
  "Use `ido-completing-read to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file ...")
    (message "Aborting")))
;; recent files (recentf):1 ends here

;; Quality of life things
;;     Additionally, let's enable a few things that just makes life
;;     better in general:

;; [[file:~/.emacs.d_new/config.org::*Quality of life things][Quality of life things:1]]
(electric-pair-mode)
;; Quality of life things:1 ends here



;; Further, let's enable line numbers everywhere and make them relative.

;; [[file:~/.emacs.d_new/config.org::*Quality of life things][Quality of life things:2]]
(global-display-line-numbers-mode)
(setq-default display-line-numbers 'relative)
;; Quality of life things:2 ends here



;; Make lines wrap visually if they're longer than what can fit in
;; the window.

;; [[file:~/.emacs.d_new/config.org::*Quality of life things][Quality of life things:3]]
(global-visual-line-mode)
;; Quality of life things:3 ends here



;; I prefer ending sentences with single spaces (even if I see the
;; benefits of using two, as recommended by Emacs), so let's also
;; tell Emacs that it's okay.

;; [[file:~/.emacs.d_new/config.org::*Quality of life things][Quality of life things:4]]
(setq sentence-end-double-space nil)
;; Quality of life things:4 ends here



;; I also want files to auto-update if they change on disk.

;; [[file:~/.emacs.d_new/config.org::*Quality of life things][Quality of life things:5]]
(global-auto-revert-mode)
;; Quality of life things:5 ends here



;; Trailing whitespace is something I prefer to avoid, so let's
;; delete that on save.

;; [[file:~/.emacs.d_new/config.org::*Quality of life things][Quality of life things:6]]
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; Quality of life things:6 ends here



;; For text modes, let's always use auto-fill mode and orgtbl-mode:

;; [[file:~/.emacs.d_new/config.org::*Quality of life things][Quality of life things:7]]
(defun my-text-mode-hook ()
  (auto-fill-mode)
  (orgtbl-mode))
(add-hook 'text-mode-hook 'my-text-mode-hook)
;; Quality of life things:7 ends here

;; Removing suspend-frame                                      :keybinding:
;;      To disable suspend-frame (I don't think I've ever meant to
;;      activate it), we'll instead replace it by a message saying it has
;;      been unmapped.

;; [[file:~/.emacs.d_new/config.org::*Removing suspend-frame][Removing suspend-frame:1]]
(global-set-key [remap suspend-frame]
  (lambda ()
    (interactive)
    (message "This keybinding is disabled (was 'suspend-frame')")))
;; Removing suspend-frame:1 ends here

;; Don't pause redisplay on input events
;;      According to [[https://www.masteringemacs.org/article/improving-performance-emacs-display-engine][Improving the performance of Emacs's Display Engine?]]
;;      by Mickey Petersen, Emacs defaults to pausing all display
;;      redrawing on any input. This may have been useful previously, but
;;      is not necessary anymore.

;; [[file:~/.emacs.d_new/config.org::*Don't pause redisplay on input events][Don't pause redisplay on input events:1]]
(setq redisplay-dont-pause t)
;; Don't pause redisplay on input events:1 ends here

;; Garbage collection
;;     Inspired by the [[https://github.com/lewang/flx][flx's readme]], we can increase the GC threshold
;;     significantly from the default. In addition to when using flx,
;;     this might also come in handy in other situations.

;; [[file:~/.emacs.d_new/config.org::*Garbage collection][Garbage collection:1]]
(setq gc-cons-threshold 20000000)
;; Garbage collection:1 ends here

;; macOS                                                            :macos:
;;      On macOS, I want the CMD key to act as Meta, and the Alt key as super, because this works better with their location on the keyboard and their corresponding keys on Linux.

;; [[file:~/.emacs.d_new/config.org::*macOS][macOS:1]]
(when (eq system-type 'darwin)
  (setq mac-option-modifier nil
        mac-command-modifier 'meta))
;; macOS:1 ends here

;; Ivy                                                            :keybinding:
;;   Following the [[https://oremacs.com/swiper/][Ivy Documentation]], let's install Ivy (and Counsel and
;;   Swiper) using counsel. We'll also replace normal isearch with Ivy's
;;   ~swiper-isearch~ and the regular ~yank-pop~ functionality with
;;   ~counsel-yank-pop~.

;;   For searches, I also prefer using a fuzzier regex pattern than the
;;   default, so this is set up by assigning ~ivy--regex-fuzzy~.


;; [[file:~/.emacs.d_new/config.org::*Ivy][Ivy:1]]
(use-package counsel
  :demand
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
	ivy-count-format "(%d/%d)"
	ivy-re-builders-alist '((swiper-isearch . ivy--regex-plus)
				(t . ivy--regex-fuzzy))
	ivy-wrap t)
  :bind (("C-S-s" . swiper-isearch)
	 ("C-M-y" . counsel-yank-pop)))
;; Ivy:1 ends here

;; Flycheck

;; [[file:~/.emacs.d_new/config.org::*Flycheck][Flycheck:1]]
(use-package flycheck
  :ensure t
  :config (global-flycheck-mode))
;; Flycheck:1 ends here

;; TODO Company                                                   :keybinding:
;;   Completion is important! [[https://github.com/company-mode/company-mode][Company-mode repo]]

;;   We need to set this up properly. In addition to wanting to use
;;   tab-n-go, I also want to be able to use TAB to expand completions
;;   with function arguments (like Rust Analyzer).

;;   In addition to the basic company-mode configuration, I also use
;;   [[https://github.com/company-mode/company-mode/blob/master/company-tng.el][company-tng]] to get YCMD-like behavior.  This requires a little extra
;;   bit of configuration to get set up.


;; [[file:~/.emacs.d_new/config.org::*Company][Company:1]]
(defun my-company-tng-setup ()
  (setq company-require-match nil
	company-frontends '(company-tng-frontend
			    company-pseudo-tooltip-frontend
			    company-echo-metadata-frontend))
  (let ((keymap company-active-map))
    (define-key keymap [return] nil)
    (define-key keymap (kbd "RET") nil)))
(use-package company
  :config
  (global-company-mode)
  (global-set-key (kbd "C-<f5>") 'company-complete)
  (my-company-tng-setup)
  (setq company-idle-delay 0.2
	company-selection-wrap-around t))
;; Company:1 ends here



;; When using varying font sizes within a buffer, this can make the
;; company completion dropdown misaligned (with itself, even). Using
;; the [[https://github.com/tumashu/company-posframe][company-posframe package]] appears to fix this by putting
;; completions in a separate frame.

;; [[file:~/.emacs.d_new/config.org::*Company][Company:2]]
(use-package company-posframe
  :after company
  :config (company-posframe-mode 1))
;; Company:2 ends here



;; To make completions a bit smoother, I use [[https://github.com/PythonNut/company-flx][company-flx]] to allow for
;; fuzzy matching when company uses the company-capf backend. I'll also
;; add ~fuzzy~ to the list of completion styles.

;; [[file:~/.emacs.d_new/config.org::*Company][Company:3]]
(use-package company-flx
  :after company
  :config
  (company-flx-mode 1)
  (add-to-list 'completion-styles 'fuzzy))
;; Company:3 ends here

;; TODO Magit                                                     :keybinding:
;;   Because it's simply one of the best git experiences out there, of
;;   course I want to enable and use Magit:

;; [[file:~/.emacs.d_new/config.org::*Magit][Magit:1]]
(use-package magit
  :bind (("C-x g" . magit-status)
	 ("C-x M-g" . magit-dispatch)))
;; Magit:1 ends here



;; Additionally, to smooth out the workflow with GitHub and GitLab,
;; let's also use Forge.

;; Note: this is currently commented out because Forge seems to throw
;; errors when I've already got spacemacs set up in
;; ~user-emacs-directory~. Comment this back in when I switch over.

;; [[file:~/.emacs.d_new/config.org::*Magit][Magit:2]]
;; (use-package forge
;;   :after magit
;;   :demand
;;   :config
;;   (add-to-list 'forge-alist
;; 	       '("gitlab.intility.no" "gitlab.intility.no/api/v4" "gitlab.intility.no" forge-gitlab-repository))
;;   :bind (:map magit-mode-map
;; 	      ("C-c M-w" . forge-copy-url-at-point-as-kill)))
;; Magit:2 ends here

;; Rainbow delimiters
;;   Rainbow delimiters make it much easier to read a lot of code, so
;;   let's set them up.

;; [[file:~/.emacs.d_new/config.org::*Rainbow delimiters][Rainbow delimiters:1]]
(use-package rainbow-delimiters
  :hook ((prog-mode text-mode) . rainbow-delimiters-mode))
;; Rainbow delimiters:1 ends here

;; Nix mode                                                       :keybinding:
;;   :PROPERTIES:
;;   :REPO:     [[https://github.com/NixOS/nix-mode/][GitHub]]
;;   :END:

;;   Using NixOS as my main OS and Nix shells for dev environments on
;;   other platforms, it's quite important to get this mode set up.

;;   Of note: nix-mode seems to have a dependency on json-mode, so make
;;   sure to only initialize after said mode.

;; [[file:~/.emacs.d_new/config.org::*Nix mode][Nix mode:1]]
(use-package nix-mode
  :after json-mode
  :mode "\\.nix\\'"
  :bind (:map nix-mode-map
	      ("C-c C-f" . nix-format-buffer)))
;; Nix mode:1 ends here

;; JSON mode
;;   :PROPERTIES:
;;   :REPO:      [[https://github.com/joshwnj/json-mode][GitHub]]
;;   :END:
;;   For JSON support and, more specifically, for Nix mode, which seems
;;   to require this.


;; [[file:~/.emacs.d_new/config.org::*JSON mode][JSON mode:1]]
(use-package json-mode)
;; JSON mode:1 ends here

;; TODO Change inner                                              :keybinding:
;;   Note: this could do with some improvements to also accept closing parens,
;;   braces, brackets, etc., and to allow certain shortcuts, such as ~b~ for
;;   ~parens~. Should actually be fairly doable. This has been reported previously
;;   ([[https://github.com/magnars/change-inner.el/issues/8][issue]]) and been deemed not worth doing. Another option is [[https://gist.github.com/alphapapa/fd7edf8104215028f3da][this gist]] by
;;   Alphapapa, which could work well.

;;   To emulate some of Vim's behavior, we'll use [[https://github.com/magnars/change-inner.el][change-inner.el]]:

;; [[file:~/.emacs.d_new/config.org::*Change inner][Change inner:1]]
(use-package change-inner
  :bind (("M-i" . change-inner)
	 ("M-o" . change-outer)))
;; Change inner:1 ends here

;; Spell checker
;;   Enable regular spell checking in all text modes and prog type spell
;;   checking in prog modes:

;; [[file:~/.emacs.d_new/config.org::*Spell checker][Spell checker:1]]
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'text-mode-hook 'flyspell-mode)
;; Spell checker:1 ends here

;; Markdown mode
;;   :PROPERTIES:
;;   :REPO:     [[https://github.com/jrblevin/markdown-mode][GitHub]]
;;   :END:


;; [[file:~/.emacs.d_new/config.org::*Markdown mode][Markdown mode:1]]
(use-package markdown-mode
  :mode (("\\.md\\'" . gfm-mode))
  :after
  (edit-indirect)
  :config
  (setq markdown-asymmetric-header t))
;; Markdown mode:1 ends here

;; Edit-indirect
;;   :PROPERTIES:
;;   :REPO:     [[https://github.com/Fanael/edit-indirect][GitHub]]
;;   :END:

;;   To edit arbitrary code in separate buffers (the same way org mode
;;   does), the package edit-indirect exist. This is required by markdown
;;   mode to edit source code blocks in separate buffers.


;; [[file:~/.emacs.d_new/config.org::*Edit-indirect][Edit-indirect:1]]
(use-package edit-indirect)
;; Edit-indirect:1 ends here
