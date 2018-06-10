;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
    ;; Base distribution to use. This is a layer contained in the directory
    ;; `+distribution'. For now available distributions are `spacemacs-base'
    ;; or `spacemacs'. (default 'spacemacs)
    dotspacemacs-distribution 'spacemacs
    ;; Lazy installation of layers (i.e. layers are installed only when a file
    ;; with a supported type is opened). Possible values are `all', `unused'
    ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
    ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
    ;; lazy install any layer that support lazy installation even the layers
    ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
    ;; installation feature and you have to explicitly list a layer in the
    ;; variable `dotspacemacs-configuration-layers' to install it.
    ;; (default 'unused)
    dotspacemacs-enable-lazy-installation 'unused
    ;; If non-nil then Spacemacs will ask for confirmation before installing
    ;; a layer lazily. (default t)
    dotspacemacs-ask-for-lazy-installation t
    ;; If non-nil layers with lazy install support are lazy installed.
    ;; List of additional paths where to look for configuration layers.
    ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
    dotspacemacs-configuration-layer-path '()
    ;; List of configuration layers to load.
    dotspacemacs-configuration-layers
    '(vimscript
       yaml
       python
       javascript
       html
       rust
       common-lisp
       elm
       reason
       ocaml
       ;; ----------------------------------------------------------------
       ;; Example of useful layers you may want to use right away.
       ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
       ;; <M-m f e R> (Emacs style) to install them.
       ;; ----------------------------------------------------------------
       helm
       auto-completion
       ;; better-defaults
       emacs-lisp
       git
       markdown
       (org :variables org-want-todo-bindings t)

       ;; (shell :variables
       ;;        shell-default-width 80
       ;;        shell-default-position 'right)
       ;; spell-checking
       syntax-checking
       version-control
       )
    ;; List of additional packages that will be installed without being
    ;; wrapped in a layer. If you need some configuration for these
    ;; packages, then consider creating a layer. You can also put the
    ;; configuration in `dotspacemacs/user-config'.
    dotspacemacs-additional-packages '(all-the-icons
                                        atom-one-dark-theme
                                        company-flx
                                        color-theme-sanityinc-tomorrow
                                        editorconfig
                                        gitter
                                        js-format
                                        eslintd-fix
                                        lsp-rust
                                        p4
                                        zerodark-theme)
    ;; A list of packages that cannot be updated.
    dotspacemacs-frozen-packages '()
    ;; A list of packages that will not be installed and loaded.
    dotspacemacs-excluded-packages '(vi-tilde-fringe)
    ;; Defines the behaviour of Spacemacs when installing packages.
    ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
    ;; `used-only' installs only explicitly used packages and uninstall any
    ;; unused packages as well as their unused dependencies.
    ;; `used-but-keep-unused' installs only the used packages but won't uninstall
    ;; them if they become unused. `all' installs *all* packages supported by
    ;; Spacemacs and never uninstall them. (default is `used-only')
    dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
    ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
    ;; possible. Set it to nil if you have no way to use HTTPS in your
    ;; environment, otherwise it is strongly recommended to let it set to t.
    ;; This variable has no effect if Emacs is launched with the parameter
    ;; `--insecure' which forces the value of this variable to nil.
    ;; (default t)
    dotspacemacs-elpa-https t
    ;; Maximum allowed time in seconds to contact an ELPA repository.
    dotspacemacs-elpa-timeout 5
    ;; If non nil then spacemacs will check for updates at startup
    ;; when the current branch is not `develop'. Note that checking for
    ;; new versions works via git commands, thus it calls GitHub services
    ;; whenever you start Emacs. (default nil)
    dotspacemacs-check-for-update nil
    ;; If non-nil, a form that evaluates to a package directory. For example, to
    ;; use different package directories for different Emacs versions, set this
    ;; to `emacs-version'.
    dotspacemacs-elpa-subdirectory nil
    ;; One of `vim', `emacs' or `hybrid'.
    ;; `hybrid' is like `vim' except that `insert state' is replaced by the
    ;; `hybrid state' with `emacs' key bindings. The value can also be a list
    ;; with `:variables' keyword (similar to layers). Check the editing styles
    ;; section of the documentation for details on available variables.
    ;; (default 'vim)
    dotspacemacs-editing-style 'vim
    ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
    dotspacemacs-verbose-loading nil
    ;; Specify the startup banner. Default value is `official', it displays
    ;; the official spacemacs logo. An integer value is the index of text
    ;; banner, `random' chooses a random text banner in `core/banners'
    ;; directory. A string value must be a path to an image format supported
    ;; by your Emacs build.
    ;; If the value is nil then no banner is displayed. (default 'official)
    dotspacemacs-startup-banner 'official
    ;; List of items to show in startup buffer or an association list of
    ;; the form `(list-type . list-size)`. If nil then it is disabled.
    ;; Possible values for list-type are:
    ;; `recents' `bookmarks' `projects' `agenda' `todos'."
    ;; List sizes may be nil, in which case
    ;; `spacemacs-buffer-startup-lists-length' takes effect.
    dotspacemacs-startup-lists '((recents . 5)
                                  (projects . 7))
    ;; True if the home buffer should respond to resize events.
    dotspacemacs-startup-buffer-responsive t
    ;; Default major mode of the scratch buffer (default `text-mode')
    dotspacemacs-scratch-mode 'text-mode
    ;; List of themes, the first of the list is loaded when spacemacs starts.
    ;; Press <SPC> T n to cycle to the next theme in the list (works great
    ;; with 2 themes variants, one dark and one light)
    dotspacemacs-themes '(atom-one-dark
                           zerodark
                           spacemacs-dark)

    dotspacemacs-mode-line-theme 'all-the-icons
    ;; If non nil the cursor color matches the state color in GUI Emacs.
    dotspacemacs-colorize-cursor-according-to-state t
    ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
    ;; quickly tweak the mode-line size to make separators look not too crappy.
    dotspacemacs-default-font '("Dank Mono"
                                 :size 18
                                 :weight normal
                                 :width normal
                                 :powerline-scale 1.1)
    ;; The leader key
    dotspacemacs-leader-key "SPC"
    ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
    ;; (default "SPC")
    dotspacemacs-emacs-command-key "SPC"
    ;; The key used for Vim Ex commands (default ":")
    dotspacemacs-ex-command-key ":"
    ;; The leader key accessible in `emacs state' and `insert state'
    ;; (default "M-m")
    dotspacemacs-emacs-leader-key "M-m"
    ;; Major mode leader key is a shortcut key which is the equivalent of
    ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
    dotspacemacs-major-mode-leader-key ","
    ;; Major mode leader key accessible in `emacs state' and `insert state'.
    ;; (default "C-M-m")
    dotspacemacs-major-mode-emacs-leader-key "C-M-m"
    ;; These variables control whether separate commands are bound in the GUI to
    ;; the key pairs C-i, TAB and C-m, RET.
    ;; Setting it to a non-nil value, allows for separate commands under <C-i>
    ;; and TAB or <C-m> and RET.
    ;; In the terminal, these pairs are generally indistinguishable, so this only
    ;; works in the GUI. (default nil)
    dotspacemacs-distinguish-gui-tab nil
    ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
    dotspacemacs-remap-Y-to-y$ t
    ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
    ;; there. (default t)
    dotspacemacs-retain-visual-state-on-shift t
    ;; If non-nil, J and K move lines up and down when in visual mode.
    ;; (default nil)
    dotspacemacs-visual-line-move-text nil
    ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
    ;; (default nil)
    dotspacemacs-ex-substitute-global nil
    ;; Name of the default layout (default "Default")
    dotspacemacs-default-layout-name "Default"
    ;; If non nil the default layout name is displayed in the mode-line.
    ;; (default nil)
    dotspacemacs-display-default-layout nil
    ;; If non nil then the last auto saved layouts are resume automatically upon
    ;; start. (default nil)
    dotspacemacs-auto-resume-layouts t
    ;; Size (in MB) above which spacemacs will prompt to open the large file
    ;; literally to avoid performance issues. Opening a file literally means that
    ;; no major mode or minor modes are active. (default is 1)
    dotspacemacs-large-file-size 1
    ;; Location where to auto-save files. Possible values are `original' to
    ;; auto-save the file in-place, `cache' to auto-save the file to another
    ;; file stored in the cache directory and `nil' to disable auto-saving.
    ;; (default 'cache)
    dotspacemacs-auto-save-file-location 'original
    ;; Maximum number of rollback slots to keep in the cache. (default 5)
    dotspacemacs-max-rollback-slots 5
    ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
    dotspacemacs-helm-resize nil
    ;; if non nil, the helm header is hidden when there is only one source.
    ;; (default nil)
    dotspacemacs-helm-no-header nil
    ;; define the position to display `helm', options are `bottom', `top',
    ;; `left', or `right'. (default 'bottom)
    dotspacemacs-helm-position 'bottom
    ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
    ;; in all non-asynchronous sources. If set to `source', preserve individual
    ;; source settings. Else, disable fuzzy matching in all sources.
    ;; (default 'always)
    dotspacemacs-helm-use-fuzzy 'always
    ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
    ;; several times cycle between the kill ring content. (default nil)
    dotspacemacs-enable-paste-transient-state nil
    ;; Which-key delay in seconds. The which-key buffer is the popup listing
    ;; the commands bound to the current keystroke sequence. (default 0.4)
    dotspacemacs-which-key-delay 0.4
    ;; Which-key frame position. Possible values are `right', `bottom' and
    ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
    ;; right; if there is insufficient space it displays it at the bottom.
    ;; (default 'bottom)
    dotspacemacs-which-key-position 'bottom
    ;; If non nil a progress bar is displayed when spacemacs is loading. This
    ;; may increase the boot time on some systems and emacs builds, set it to
    ;; nil to boost the loading time. (default t)
    dotspacemacs-loading-progress-bar t
    ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
    ;; (Emacs 24.4+ only)
    dotspacemacs-fullscreen-at-startup nil
    ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
    ;; Use to disable fullscreen animations in OSX. (default nil)
    dotspacemacs-fullscreen-use-non-native nil
    ;; If non nil the frame is maximized when Emacs starts up.
    ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
    ;; (default nil) (Emacs 24.4+ only)
    dotspacemacs-maximized-at-startup nil
    ;; A value from the range (0..100), in increasing opacity, which describes
    ;; the transparency level of a frame when it's active or selected.
    ;; Transparency can be toggled through `toggle-transparency'. (default 90)
    dotspacemacs-active-transparency 90
    ;; A value from the range (0..100), in increasing opacity, which describes
    ;; the transparency level of a frame when it's inactive or deselected.
    ;; Transparency can be toggled through `toggle-transparency'. (default 90)
    dotspacemacs-inactive-transparency 90
    ;; If non nil show the titles of transient states. (default t)
    dotspacemacs-show-transient-state-title t
    ;; If non nil show the color guide hint for transient state keys. (default t)
    dotspacemacs-show-transient-state-color-guide t
    ;; If non nil unicode symbols are displayed in the mode line. (default t)
    dotspacemacs-mode-line-unicode-symbols t
    ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
    ;; scrolling overrides the default behavior of Emacs which recenters point
    ;; when it reaches the top or bottom of the screen. (default t)
    dotspacemacs-smooth-scrolling t
    ;; Control line numbers activation.
    ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
    ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
    ;; This variable can also be set to a property list for finer control:
    ;; '(:relative nil
    ;;   :disabled-for-modes dired-mode
    ;;                       doc-view-mode
    ;;                       markdown-mode
    ;;                       org-mode
    ;;                       pdf-view-mode
    ;;                       text-mode
    ;;   :size-limit-kb 1000)
    ;; (default nil)
    dotspacemacs-line-numbers '(:relative t)
    ;; Code folding method. Possible values are `evil' and `origami'.
    ;; (default 'evil)
    dotspacemacs-folding-method 'evil
    ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
    ;; (default nil)
    dotspacemacs-smartparens-strict-mode nil
    ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
    ;; over any automatically added closing parenthesis, bracket, quote, etc…
    ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
    dotspacemacs-smart-closing-parenthesis nil
    ;; Select a scope to highlight delimiters. Possible values are `any',
    ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
    ;; emphasis the current one). (default 'all)
    dotspacemacs-highlight-delimiters 'all
    ;; If non nil, advise quit functions to keep server open when quitting.
    ;; (default nil)
    dotspacemacs-persistent-server t
    ;; List of search tool executable names. Spacemacs uses the first installed
    ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
    ;; (default '("ag" "pt" "ack" "grep"))
    dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
    ;; The default package repository used if no explicit repository has been
    ;; specified with an installed package.
    ;; Not used for now. (default nil)
    dotspacemacs-default-package-repository nil
    ;; Delete whitespace while saving buffer. Possible values are `all'
    ;; to aggressively delete empty line and long sequences of whitespace,
    ;; `trailing' to delete only the whitespace at end of lines, `changed'to
    ;; delete only whitespace for changed lines or `nil' to disable cleanup.
    ;; (default nil)
    dotspacemacs-whitespace-cleanup 'trailing
    ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."
  (setq vc-follow-symlinks t

    ;; file system
    create-lockfiles nil

    ;; key bindings
    dotspacemacs-distinguish-gui-tab t ;; to differentiate between C-i and tab

    ;; whitespace
    whitespace-style '(face spaces tabs newline indentation space-mark tab-mark)

    ;; centered buffer mode
    spacemacs-centered-buffer-mode-min-content-width 1200
    spacemacs-centered-buffer-mode-max-content-width 1200

    ;; company
    company-flx-limit 50
    company-idle-delay 0
    company-minimum-prefix-length 1
    company-selection-wrap-around t
    company-tooltip-align-annotations t
    company-frontends '(company-pseudo-tooltip-unless-just-one-frontend company-echo-metadata-frontend company-preview-if-just-one-frontend company-tng-frontend)

    ;; auto-completion
    tab-always-indent t
    auto-completion-return-key-behavior nil
    auto-completion-tab-key-behavior nil
    auto-completion-enable-snippets-in-popup t
    auto-completion-enable-help-tooltip t
    auto-completion-enable-sort-by-usage t

    ;; neotree
    neo-confirm-create-file 'off-p
    neo-confirm-create-directory 'off-p
    neo-confirm-delete-directory-recursively 'off-p
    neo-confirm-delete-file 'off-p
    neo-confirm-kill-buffers-for-files-in-directory 'off-p
    neo-theme (if (display-graphic-p) 'icons 'arrow)

    ;; elm-lang
    elm-format-on-save t
    elm-tags-on-save t
    elm-sort-imports-on-save t

    ;; javascript js
    js2-strict-missing-semi-warning nil
    js-indent-level 2
    flycheck-javascript-eslint-executable "eslint_d"

    ;; mac-specific
    mac-use-title-bar t

    ;; rust-lang
    rust-format-on-save t)


  ;; editorconfig
  (editorconfig-mode t)

  ;; (add-to-list 'flycheck-checkers 'javascript-eslint)
  (setq-default flycheck-disabled-checkers
    (append flycheck-disabled-checkers
      '(javascript-jshint)))
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (add-hook 'js2-mode-hook 'eslintd-fix-mode)

  ;; evil
  (evil-define-key 'visual evil-surround-mode-map "S" 'evil-surround-region)

  ;; skewer mode
  (add-hook 'css-mode-hook 'skewer-reload-stylesheets-start-editing)

  ;; lines
  (global-visual-line-mode t)

  ;; motions
  (spacemacs/toggle-camel-case-motion-globally-on)

  ;; text objects
  (defmacro define-and-bind-text-object (key start-regex end-regex)
    (let ((inner-name (make-symbol "inner-line"))
           (outer-name (make-symbol "outer-line")))
      `(progn
         (evil-define-text-object ,inner-name (count &optional beg end type)
           (evil-select-paren ,start-regex ,end-regex beg end type count nil))
         (evil-define-text-object ,outer-name (count &optional beg end type)
           (evil-select-paren ,start-regex ,end-regex beg end type count t))
         (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
         (define-key evil-outer-text-objects-map ,key (quote ,outer-name)))))
  ;; create "il"/"al" (inside/around) line text objects:
  (define-and-bind-text-object "l" "^\\s-*" "\\s-*$")

  (evil-leader/set-key "/" 'spacemacs/helm-project-do-ag)
  (evil-define-key 'normal global-map (kbd "C-a") 'evil-numbers/inc-at-pt)
  (evil-define-key 'normal global-map (kbd "C-x") 'evil-numbers/dec-at-pt)
  (evil-define-key 'normal global-map (kbd "C-<tab>") 'next-buffer)
  (evil-define-key 'normal global-map (kbd "C-S-<tab>") 'previous-buffer)
  (evil-define-key '(normal visual) global-map (kbd "j") 'evil-next-visual-line)
  (evil-define-key '(normal visual) global-map (kbd "k") 'evil-previous-visual-line)
  (evil-define-key 'insert global-map (kbd "C-h") 'evil-delete-backward-char)
  (define-key evil-outer-text-objects-map "e" 'evil-inner-buffer)
  (define-key evil-inner-text-objects-map "e" 'evil-inner-buffer)
  (setq-default evil-escape-key-sequence "+]")
  (setq case-fold-search nil)

  (setq evil-v$-gets-eol nil)

  ;; stop `v$' from selecting newlines
  (evil-define-motion evil-end-of-line (count)
    "Move the cursor to the end of the current line.
If COUNT is given, move COUNT - 1 lines downward first."
    :type inclusive
    (move-end-of-line count)
    (when evil-track-eol
      (setq temporary-goal-column most-positive-fixnum
        this-command 'next-line))
    (unless (and (evil-visual-state-p) evil-v$-gets-eol)
      (evil-adjust-cursor)
      (when (eolp)
        ;; prevent "c$" and "d$" from deleting blank lines
        (setq evil-this-type 'exclusive))))

  (spacemacs/set-leader-keys
    "," 'ace-delete-window
    "-" 'split-window-below-and-focus
    "." 'ace-window
    "\\" 'split-window-right-and-focus
    "gc" 'magit-commit
    "gd" 'magit-diff-popup
    "gp" 'magit-push
    "jt" 'avy-goto-char-timer
    "o" 'helm-projectile-find-file)

  ;; css mode
  (spacemacs/set-leader-keys-for-major-mode 'css-mode "i" 'impatient-mode)

  ;;magit
  ;; (add-hook 'git-commit-mode-hook (lambda () (save-selected-window (magit-process))))
  (global-company-mode)
  (with-eval-after-load 'company
    (company-flx-mode +1)
    (define-key company-active-map (kbd "C-w") 'evil-delete-backward-word)
    (define-key company-active-map (kbd "C-h") 'evil-delete-backward-char)
    (define-key company-active-map (kbd "C-d") 'company-show-doc-buffer)
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous))
  ;; (define-key company-quickhelp-mode-map (kbd "C-n") 'company-select-next)
  ;; (define-key company-quickhelp-mode-map (kbd "C-p") 'company-select-previous))

  (with-eval-after-load 'helm
    (dolist (keymap (list helm-find-files-map helm-read-file-map))
      (define-key keymap (kbd "C-w") 'helm-find-files-up-one-level)
      (define-key helm-map (kbd "C-w") 'evil-delete-backward-word)))

  ;; Make C-/ expand yasnippet if available, else go into company
  ;; Must unbind undo tree first
  (with-eval-after-load 'undo-tree
    (define-key undo-tree-map (kbd "C-/") nil))
  (defun nir-yasnippet-expand-or-complete ()
    (interactive)
    (unless (call-interactively 'yas-expand) (call-interactively 'company-yasnippet)))
  ;; Must bind in global map, else undo tree stops loading
  (with-eval-after-load 'yasnippet
    (define-key global-map (kbd "C-/") 'nir-yasnippet-expand-or-complete))

  ;; ligatures
  (mac-auto-operator-composition-mode)

  ;; add new file type associations
  ;; (add-to-list 'auto-mode-alist '("\\.gd$" . python-mode))

  ;; Fira code
  ;; (when (window-system)
  ;;   (set-frame-font "Fira Code"))
  ;; (let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
  ;;                 (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
  ;;                 (36 . ".\\(?:>\\)")
  ;;                 (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
  ;;                 (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
  ;;                 (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
  ;;                 (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
  ;;                 (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
  ;;                 (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
  ;;                 (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
  ;;                 (48 . ".\\(?:x[a-zA-Z]\\)")
  ;;                 (58 . ".\\(?:::\\|[:=]\\)")
  ;;                 (59 . ".\\(?:;;\\|;\\)")
  ;;                 (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
  ;;                 (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
  ;;                 (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
  ;;                 (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
  ;;                 (91 . ".\\(?:]\\)")
  ;;                 (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
  ;;                 (94 . ".\\(?:=\\)")
  ;;                 (119 . ".\\(?:ww\\)")
  ;;                 (123 . ".\\(?:-\\)")
  ;;                 (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
  ;;                 (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
  ;;                 )
  ;;         ))
  ;;   (dolist (char-regexp alist)
  ;;     (set-char-table-range composition-function-table (car char-regexp)
  ;;       `([,(cdr char-regexp) 0 font-shape-gstring]))))

  ;; (add-hook 'helm-major-mode-hook
  ;;   (lambda ()
  ;;     (setq auto-composition-mode nil)))

  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  '(package-selected-packages
     (quote
       (zoom-window magit-p4 p4 company-flx editorconfig js-format gitter slime-company slime common-lisp-snippets web-beautify livid-mode skewer-mode simple-httpd json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc company-tern dash-functional tern coffee-mode memoize all-the-icons company-quickhelp git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter diff-hl company-web web-completion-data web-mode tagedit slim-mode scss-mode sass-mode pug-mode less-css-mode helm-css-scss haml-mode emmet-mode xterm-color shell-pop multi-term mmm-mode markdown-toc markdown-mode gh-md flyspell-correct-helm flyspell-correct flycheck-rust flycheck-pos-tip flycheck-elm flycheck eshell-z eshell-prompt-extras esh-help auto-dictionary helm-company helm-c-yasnippet fuzzy company-statistics company auto-yasnippet yasnippet ac-ispell auto-complete evil-avy atom-one-dark-theme toml-mode racer pos-tip cargo rust-mode elm-mode smeargle orgit org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-mime org-download magit-gitflow htmlize helm-gitignore gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link evil-magit magit magit-popup git-commit ghub with-editor ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint indent-guide hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu elisp-slime-nav dumb-jump diminish define-word column-enforce-mode clean-aindent-mode auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  )
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
    '(package-selected-packages
         (quote
             (eslintd-fix zoom-window magit-p4 p4 company-flx editorconfig js-format gitter slime-company slime common-lisp-snippets web-beautify livid-mode skewer-mode simple-httpd json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc company-tern dash-functional tern coffee-mode memoize all-the-icons company-quickhelp git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter diff-hl company-web web-completion-data web-mode tagedit slim-mode scss-mode sass-mode pug-mode less-css-mode helm-css-scss haml-mode emmet-mode xterm-color shell-pop multi-term mmm-mode markdown-toc markdown-mode gh-md flyspell-correct-helm flyspell-correct flycheck-rust flycheck-pos-tip flycheck-elm flycheck eshell-z eshell-prompt-extras esh-help auto-dictionary helm-company helm-c-yasnippet fuzzy company-statistics company auto-yasnippet yasnippet ac-ispell auto-complete evil-avy atom-one-dark-theme toml-mode racer pos-tip cargo rust-mode elm-mode smeargle orgit org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-mime org-download magit-gitflow htmlize helm-gitignore gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link evil-magit magit magit-popup git-commit ghub with-editor ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint indent-guide hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu elisp-slime-nav dumb-jump diminish define-word column-enforce-mode clean-aindent-mode auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
