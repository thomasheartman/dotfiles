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
    ;; or `spacemacs'. (default 'spacemacs) dotspacemacs-distribution 'spacemacs
    ;; Lazy installation of layers (i.e. layers are installed only when a file
    ;; with a supported type is opened). Possible values are `all', `unused'
    ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
    ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
    ;; lazy install any layer that support lazy installation even the layers
    ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
    ;; installation feature and you have to explicitly list a layer in the
    ;; variable `dotspacemacs-configuration-layers' to install it.
    ;; (default 'unused) dotspacemacs-enable-lazy-installation 'unused
    ;; If non-nil then Spacemacs will ask for confirmation before installing
    ;; a layer lazily. (default t)
    dotspacemacs-ask-for-lazy-installation
    t
    ;; If non-nil layers with lazy install support are lazy installed.
    ;; List of additional paths where to look for configuration layers.
    ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
    dotspacemacs-configuration-layer-path
    '()
    ;; List of configuration layers to load.
    dotspacemacs-configuration-layers
    '(
       perl5
       (auto-completion (haskell :variables haskell-completion-backend 'lsp
                          auto-completion-enable-help-tooltip t))
       autohotkey
       common-lisp
       csharp
       csv
       dhall
       docker
       elixir
       (elfeed :variables rmh-elfeed-org-files (list "~/feeds.org"))
       elm
       emacs-lisp
       emoji
       (erc :variables erc-server-list'(("irc.freenode.net" :port "6697"
                                          :ssl t
                                          :nick "t-hart")))
       exwm
       fsharp
       git
       gpu
       (haskell :variables haskell-enable-hindent-style
         "johan-tibell")
       helm
       html
       (javascript :variables node-add-modules-path
         t)
       (latex :variables latex-enable-folding
         t)
       lsp
       markdown
       (mu4e :variables mu4e-use-maildirs-extension t mu4e-enable-async-operations t)
       multiple-cursors
       nginx
       nixos
       (org :variables org-want-todo-bindings
         t org-enable-reveal-js-support nil org-html-html5-fancy t org-html-doctype "html5" org-highlight-latex-and-related '(latex script entities) org-enable-org-journal-support t)

       parinfer
       pdf
       prettier
       (python :variables python-enable-yapf-format-on-save t python-sort-imports-on-save t)
       (ranger :variables ranger-show-preview
         t)
       react
       (semantic :disabled-for emacs-lisp)
       (shell :variables shell-default-shell 'eshell shell-enable-smart-eshell t shell-default-position 'right shell-default-width 50 shell-default-term-shell (executable-find "fish") multi-term-program (executable-find "fish"))
       shell-scripts
       spell-checking
       sql
       syntax-checking
       (terraform :variables terraform-auto-format-on-save t)
       (typescript :variables typescript-fmt-on-save
         t typescript-fmt-tool 'prettier)
       version-control
       vimscript
       windows-scripts
       yaml
       )

    ;; List of additional packages that will be installed without being
    ;; wrapped in a layer. If you need some configuration for these
    ;; packages, then consider creating a layer. You can also put the
    ;; configuration in `dotspacemacs/user-config'.
    dotspacemacs-additional-packages
    '(
       all-the-icons
       atom-one-dark-theme
       color-theme-sanityinc-tomorrow
       company-flx
       company-lsp
       dash
       (direnv :config (direnv-mode))
       dotnet
       editorconfig
       eglot
       eslintd-fix
       evil-smartparens
       forge
       ht
       js-format
       lsp-ui
       mixed-pitch
       org-ref
       (reason-mode
         :location (recipe
                     :repo "reasonml-editor/reason-mode"
                     :fetcher github
                     :files ("reason-mode.el" "refmt.el" "reason-indent.el" "reason-interaction.el")))
       rustic
       toml-mode
       cargo
       zerodark-theme
       )
    ;; A list of packages that cannot be updated.
    dotspacemacs-frozen-packages
    '()
    ;; A list of packages that will not be installed and loaded.
    dotspacemacs-excluded-packages
    '(vi-tilde-fringe
       )
    ;; Defines the behaviour of Spacemacs when installing packages.
    ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
    ;; `used-only' installs only explicitly used packages and uninstall any
    ;; unused packages as well as their unused dependencies.
    ;; `used-but-keep-unused' installs only the used packages but won't uninstall
    ;; them if they become unused. `all' installs *all* packages supported by
    ;; Spacemacs and never uninstall them. (default is `used-only')
    dotspacemacs-install-packages
    'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
    ;; If non-nil, Spacemacs will not try to refresh packages on startup.
    ;; In other words, it will not try and reach out to MELPA/ELPA/other
    ;; repositories. If you need to update packages, do it manually.
    ;; configuration-layer--package-archives-refreshed t
    ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
    ;; possible. Set it to nil if you have no way to use HTTPS in your
    ;; environment, otherwise it is strongly recommended to let it set to t.
    ;; This variable has no effect if Emacs is launched with the parameter
    ;; `--insecure' which forces the value of this variable to nil.
    ;; (default t) dotspacemacs-elpa-https t
    ;; Maximum allowed time in seconds to contact an ELPA repository. dotspacemacs-elpa-timeout 5
    ;; If non nil then spacemacs will check for updates at startup
    ;; when the current branch is not `develop'. Note that checking for
    ;; new versions works via git commands, thus it calls GitHub services
    ;; whenever you start Emacs. (default nil)
    dotspacemacs-check-for-update
    nil
    ;; If non-nil, a form that evaluates to a package directory. For example, to
    ;; use different package directories for different Emacs versions, set this
    ;; to `emacs-version'.
    dotspacemacs-elpa-subdirectory
    nil
    ;; One of `vim', `emacs' or `hybrid'.
    ;; `hybrid' is like `vim' except that `insert state' is replaced by the
    ;; `hybrid state' with `emacs' key bindings. The value can also be a list
    ;; with `:variables' keyword (similar to layers). Check the editing styles
    ;; section of the documentation for details on available variables.
    ;; (default 'vim)
    dotspacemacs-editing-style
    'hybrid
    ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
    dotspacemacs-verbose-loading
    nil
    ;; Specify the startup banner. Default value is `official', it displays
    ;; the official spacemacs logo. An integer value is the index of text
    ;; banner, `random' chooses a random text banner in `core/banners'
    ;; directory. A string value must be a path to an image format supported
    ;; by your Emacs build.
    ;; If the value is nil then no banner is displayed. (default 'official)
    dotspacemacs-startup-banner
    nil
    ;; List of items to show in startup buffer or an association list of
    ;; the form `(list-type . list-size)`. If nil then it is disabled.
    ;; Possible values for list-type are:
    ;; `recents' `bookmarks' `projects' `agenda' `todos'.
    ;; List sizes may be nil, in which case
    ;; `spacemacs-buffer-startup-lists-length' takes effect.
    dotspacemacs-startup-lists
    '((recents . 5)
       (projects . 7))
    ;; True if the home buffer should respond to resize events.
    dotspacemacs-startup-buffer-responsive
    nil
    ;; Default major mode of the scratch buffer (default `text-mode')
    dotspacemacs-scratch-mode
    'org-mode
    ;; List of themes, the first of the list is loaded when spacemacs starts.
    ;; Press <SPC> T n to cycle to the next theme in the list (works great
    ;; with 2 themes variants, one dark and one light)
    dotspacemacs-themes
    '(atom-one-dark zerodark spacemacs-dark)
    dotspacemacs-mode-line-theme
    'all-the-icons
    ;; If non nil the cursor color matches the state color in GUI Emacs.
    dotspacemacs-colorize-cursor-according-to-state
    t
    ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
    ;; quickly tweak the mode-line size to make separators look not too crappy.
    dotspacemacs-default-font
    '(
       ("JetBrains Mono" :size 18)
       ("Hack" :size 18)
       ("Input Mono" :size 18)
       ("Dank Mono" :size 18
         :weight normal
         :width normal
         :powerline-scale 1.1))
    ;; The leader key
    dotspacemacs-leader-key
    "SPC"
    ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
    ;; (default "SPC")
    dotspacemacs-emacs-command-key
    "SPC"
    ;; The key used for Vim Ex commands (default ":")
    dotspacemacs-ex-command-key
    ":"
    ;; The leader key accessible in `emacs state' and `insert state'
    ;; (default "M-m")
    dotspacemacs-emacs-leader-key
    "M-m"
    ;; Major mode leader key is a shortcut key which is the equivalent of
    ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
    dotspacemacs-major-mode-leader-key
    ","
    ;; Major mode leader key accessible in `emacs state' and `insert state'.
    ;; (default "C-M-m")
    dotspacemacs-major-mode-emacs-leader-key
    "C-M-m"
    ;; These variables control whether separate commands are bound in the GUI to
    ;; the key pairs C-i, TAB and C-m, RET.
    ;; Setting it to a non-nil value, allows for separate commands under <C-i>
    ;; and TAB or <C-m> and RET.
    ;; In the terminal, these pairs are generally indistinguishable, so this only
    ;; works in the GUI. (default nil)
    dotspacemacs-distinguish-gui-tab
    nil
    ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
    dotspacemacs-remap-Y-to-y$
    t
    ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
    ;; there. (default t)
    dotspacemacs-retain-visual-state-on-shift
    t
    ;; If non-nil, J and K move lines up and down when in visual mode.
    ;; (default nil)
    dotspacemacs-visual-line-move-text
    t
    ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
    ;; (default nil)
    dotspacemacs-ex-substitute-global
    nil
    ;; Name of the default layout (default "Default")
    dotspacemacs-default-layout-name
    "Default"
    ;; If non nil the default layout name is displayed in the mode-line.
    ;; (default nil)
    dotspacemacs-display-default-layout
    nil
    ;; If non nil then the last auto saved layouts are resume automatically upon
    ;; start. (default nil)
    dotspacemacs-auto-resume-layouts
    t
    ;; Size (in MB) above which spacemacs will prompt to open the large file
    ;; literally to avoid performance issues. Opening a file literally means that
    ;; no major mode or minor modes are active. (default is 1)
    dotspacemacs-large-file-size
    1
    ;; Location where to auto-save files. Possible values are `original' to
    ;; auto-save the file in-place, `cache' to auto-save the file to another
    ;; file stored in the cache directory and `nil' to disable auto-saving.
    ;; (default 'cache)
    dotspacemacs-auto-save-file-location
    nil
    ;; Maximum number of rollback slots to keep in the cache. (default 5)
    dotspacemacs-max-rollback-slots
    5
    ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
    dotspacemacs-helm-resize
    nil
    ;; if non nil, the helm header is hidden when there is only one source.
    ;; (default nil)
    dotspacemacs-helm-no-header
    t
    ;; define the position to display `helm', options are `bottom', `top',
    ;; `left', or `right'. (default 'bottom)
    dotspacemacs-helm-position
    'bottom
    ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
    ;; in all non-asynchronous sources. If set to `source', preserve individual
    ;; source settings. Else, disable fuzzy matching in all sources.
    ;; (default 'always)
    dotspacemacs-helm-use-fuzzy
    'always
    ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
    ;; several times cycle between the kill ring content. (default nil)
    dotspacemacs-enable-paste-transient-state
    t
    ;; Which-key delay in seconds. The which-key buffer is the popup listing
    ;; the commands bound to the current keystroke sequence. (default 0.4)
    dotspacemacs-which-key-delay
    0.4
    ;; Which-key frame position. Possible values are `right', `bottom' and
    ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
    ;; right; if there is insufficient space it displays it at the bottom.
    ;; (default 'bottom)
    dotspacemacs-which-key-position
    'bottom
    ;; If non nil a progress bar is displayed when spacemacs is loading. This
    ;; may increase the boot time on some systems and emacs builds, set it to
    ;; nil to boost the loading time. (default t)
    dotspacemacs-loading-progress-bar
    nil
    ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
    ;; (Emacs 24.4+ only)
    dotspacemacs-fullscreen-at-startup
    nil
    ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
    ;; Use to disable fullscreen animations in OSX. (default nil)
    dotspacemacs-fullscreen-use-non-native
    nil
    ;; If non nil the frame is maximized when Emacs starts up.
    ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
    ;; (default nil) (Emacs 24.4+ only)
    dotspacemacs-maximized-at-startup
    nil
    ;; A value from the range (0..100), in increasing opacity, which describes
    ;; the transparency level of a frame when it's active or selected.
    ;; Transparency can be toggled through `toggle-transparency'. (default 90)
    dotspacemacs-active-transparency
    90
    ;; A value from the range (0..100), in increasing opacity, which describes
    ;; the transparency level of a frame when it's inactive or deselected.
    ;; Transparency can be toggled through `toggle-transparency'. (default 90)
    dotspacemacs-inactive-transparency
    90
    ;; If non nil show the titles of transient states. (default t)
    dotspacemacs-show-transient-state-title
    t
    ;; If non nil show the color guide hint for transient state keys. (default t)
    dotspacemacs-show-transient-state-color-guide
    t
    ;; If non nil unicode symbols are displayed in the mode line. (default t)
    dotspacemacs-mode-line-unicode-symbols
    t
    ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
    ;; scrolling overrides the default behavior of Emacs which recenters point
    ;; when it reaches the top or bottom of the screen. (default t)
    dotspacemacs-smooth-scrolling
    t
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
    dotspacemacs-line-numbers
    '(:relative t)
    ;; Code folding method. Possible values are `evil' and `origami'.
    ;; (default 'evil)
    dotspacemacs-folding-method
    'evil
    ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
    ;; (default nil)
    dotspacemacs-smartparens-strict-mode
    nil
    ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
    ;; over any automatically added closing parenthesis, bracket, quote, etc…
    ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
    dotspacemacs-smart-closing-parenthesis
    nil
    ;; Select a scope to highlight delimiters. Possible values are `any',
    ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
    ;; emphasis the current one). (default 'all)
    dotspacemacs-highlight-delimiters
    'all
    ;; If non nil, advise quit functions to keep server open when quitting.
    ;; (default nil)
    dotspacemacs-persistent-server
    t
    ;; List of search tool executable names. Spacemacs uses the first installed
    ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
    ;; (default '("ag" "pt" "ack" "grep"))
    dotspacemacs-search-tools
    '("ag" "pt" "ack" "grep")
    ;; The default package repository used if no explicit repository has been
    ;; specified with an installed package.
    ;; Not used for now. (default nil)
    dotspacemacs-default-package-repository
    nil
    ;; Delete whitespace while saving buffer. Possible values are `all'
    ;; to aggressively delete empty line and long sequences of whitespace,
    ;; `trailing' to delete only the whitespace at end of lines, `changed'to
    ;; delete only whitespace for changed lines or `nil' to disable cleanup.
    ;; (default nil)
    dotspacemacs-whitespace-cleanup
    'trailing))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first.")

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."
  (message "%s" "Starting user config.")
  (setq frame-resize-pixelwise t)
  (setq vc-follow-symlinks t
    ;; file system
    create-lockfiles
    nil
    ;; key bindings
    dotspacemacs-distinguish-gui-tab
    t ;; to differentiate between C-i and tab

    ;; whitespace
    whitespace-style
    '(face spaces tabs newline indentation space-mark
       tab-mark)
    ;; centered buffer mode
    spacemacs-centered-buffer-mode-min-content-width
    1200
    spacemacs-centered-buffer-mode-max-content-width
    1200
    ;; company
    company-flx-limit
    50
    company-idle-delay
    0.1
    company-minimum-prefix-length
    1
    company-selection-wrap-around
    t
    company-tooltip-align-annotations
    t
    company-frontends
    '(company-pseudo-tooltip-unless-just-one-frontend
       company-echo-metadata-frontend company-preview-if-just-one-frontend
       company-tng-frontend)
    ;; auto-completion
    tab-always-indent
    t
    auto-completion-return-key-behavior
    nil
    auto-completion-tab-key-behavior
    nil
    auto-completion-enable-snippets-in-popup
    nil
    auto-completion-enable-help-tooltip
    t
    auto-completion-enable-sort-by-usage
    t
    ;; neotree
    neo-confirm-create-file
    'off-p
    neo-confirm-create-directory
    'off-p
    neo-confirm-delete-directory-recursively
    'off-p
    neo-confirm-delete-file
    'off-p
    neo-confirm-kill-buffers-for-files-in-directory
    'off-p
    neo-theme
    (if (display-graphic-p)
      'icons
      'arrow)
    ;; elm-lang
    elm-format-on-save
    t
    elm-tags-on-save
    t
    elm-sort-imports-on-save
    t
    ;; javascript js
    js2-strict-missing-semi-warning
    nil
    js-indent-level
    2
    css-indent-offset
    2
    flycheck-javascript-eslint-executable
    "eslint_d"
    ;; mac-specific
    mac-use-title-bar
    t

    ;; when entering text with an active selection, overwrite the selection with
    ;; the input
    delete-selection-mode t

    tags-add-tables nil)
  ;; lines
  (global-visual-line-mode t)

  ;; mode line
  (setq powerline-default-separator nil)

  ;; motions
  (message "%s" "Starting motions configuration.")
  (spacemacs/toggle-camel-case-motion-globally-on)


  ;;----------------------------------------------------------------------------
  ;; nix setup
  ;;----------------------------------------------------------------------------
  (message "%s" "Starting nix setup")
  (with-eval-after-load 'nix-mode
    (setq nix-indent-function #'nix-indent-line))

  (spacemacs/set-leader-keys-for-major-mode
    'nix-mode "=" 'nix-format-buffer)
  (spacemacs/set-leader-keys-for-major-mode
    'nix-mode "f" 'nix-format-buffer)
  (add-hook 'nix-mode
    (lambda ()
      (add-hook 'before-save-hook 'nix-format-buffer nil 'local)))
  (message "%s" "Nix setup complete.")
  ;;----------------------------------------------------------------------------
  ;; end nix setup
  ;;----------------------------------------------------------------------------

  (evil-define-key '(hybrid insert) global-map (kbd "C-,") 'evil-shift-left-line)
  (evil-define-key '(hybrid insert) global-map (kbd "C-.") 'evil-shift-right-line)

  (with-eval-after-load 'helm
    (define-key helm-find-files-map (kbd "C-<backspace>") 'clean-aindent--bsunindent))

  (evil-define-key 'hybrid global-map (kbd "C-o") 'evil-open-below)
  (evil-define-key 'hybrid global-map (kbd "C-S-o") 'evil-open-above)
  (message "%s" "Finished motions configuration.")

  ;; elfeed
  (setq-default elfeed-search-filter "@2-weeks-ago +unread")
  (message "%s" "Configured elfeed.")

  ;;----------------------------------------------------------------------------
  ;; yaml setup
  ;;----------------------------------------------------------------------------

  (message "Configuring yaml mode.")
  (with-eval-after-load 'yaml-mode
    (add-hook 'yaml-mode-hook
      (lambda ()
        (whitespace-mode))))

  (message "Finished configuring yaml mode.")
  ;;----------------------------------------------------------------------------
  ;; end yaml setup
  ;;----------------------------------------------------------------------------


  ;;----------------------------------------------------------------------------
  ;; terraform setup
  ;;----------------------------------------------------------------------------
  (spacemacs/set-leader-keys-for-major-mode
    'terraform-mode "f" 'terraform-format-buffer)

  ;;----------------------------------------------------------------------------
  ;; end terraform setup
  ;;----------------------------------------------------------------------------

  ;;----------------------------------------------------------------------------
  ;; EXWM setup
  ;;----------------------------------------------------------------------------

  (message "%s" "Configuring EXWM")
  (setq
    exwm-workspace-show-all-buffers t
    exwm-layout-show-all-buffers t)
  (setq exwm-input-global-keys
    `(([?\s-r] . exwm-reset)
       ([?\s- ] . spacemacs/exwm-app-launcher)

       ;; workspaces
       ([?\s-w] . exwm-workspace-switch)
       ([?\s-s] . exwm-workspace-move-window)

       ([?\s-}]  . spacemacs/exwm-workspace-next)
       ([?\s-{]  . spacemacs/exwm-workspace-prev)

       ;; buffer management
       ([?\s-n] . next-buffer)
       ([?\s-p] . previous-buffer)

       ;; window navigation
       ([?\s-k] . evil-window-up)
       ([?\s-j] . evil-window-down)
       ([?\s-l] . evil-window-right)
       ([?\s-h] . evil-window-left)

       ([?\s-m] . spacemacs/toggle-maximize-buffer)
       ([?\s-b] . lazy-helm/helm-mini)

       ;; other
       ([?\s-c] . org-capture)

       ;; window splitting
       ([?\s-|] . split-window-right-and-focus)
       ([?\s-_] . split-window-below-and-focus)

       ;; workspace / window numerical
       ,@(mapcar (lambda (i)
                   `(,(kbd (format "s-C-%d" i)) .
                      (lambda ()
                        (interactive)
                        (exwm-workspace-switch-create ,i))))
           (number-sequence 0 9))
       ,@(mapcar (lambda (i)
                   `(,(kbd (format "s-%d" i)) .
                      (lambda ()
                        (interactive)
                        (winum-select-window-by-number ,i))))
           (number-sequence 0 9))))


  (exwm-input-set-key (kbd "s-C-M-s")
    (lambda ()
      (interactive)
      (shell-command "systemctl suspend")))

  (exwm-input-set-key (kbd "s-C-M-h")
    (lambda ()
      (interactive)
      (shell-command "systemctl hibernate")))

  (exwm-input-set-key (kbd "s-C-M-o")
    (lambda ()
      (interactive)
      (shell-command "xset dpms force off")))


  (exwm-input-set-key (kbd "s-C-M-p")
    (lambda ()
      (interactive)
      (shell-command "systemctl poweroff")))

  (exwm-input-set-key (kbd "s-C-M-r")
    (lambda ()
      (interactive)
      (shell-command "systemctl reboot")))


  (require 'exwm-randr)
  (setq exwm-randr-workspace-monitor-plist '(0 "DP2" 1 "eDP1" 2 "DP1"))
  (defun my-exwm-randr-screen-change-hook ()
    (interactive)
    (start-process-shell-command
      "autorandr" nil "autorandr -c"))
  (add-hook 'exwm-randr-screen-change-hook
    (lambda ()
      (my-exwm-randr-screen-change-hook)))
  ;; (add-hook 'exwm-randr-screen-change-hook
  ;;  (my-exwm-randr-screen-change-hook))
  (exwm-randr-enable)
  (message "%s" "Configured EXWM")

  ;;----------------------------------------------------------------------------
  ;; end EXWM setup
  ;;----------------------------------------------------------------------------

  ;;----------------------------------------------------------------------------
  ;; SQL setup
  ;;----------------------------------------------------------------------------

  (message "%s" "Configuring SQL")
  (setq
    sql-ms-program "mssql-cli"
    sql-ms-options '("-E" "--less-chatty")
    sql-ms-login-params '(server))
  (with-eval-after-load 'sql
    (sql-set-product-feature 'ms :prompt-cont-regexp "^.*>$")
    (sql-set-product-feature 'ms :prompt-regexp "^.*>$"))
  (message "%s" "Configured SQL")

  ;;----------------------------------------------------------------------------
  ;; SQL setup
  ;;----------------------------------------------------------------------------
  ;;----------------------------------------------------------------------------
  ;; email setup
  ;;----------------------------------------------------------------------------
  ;; find mu4e path on NixOS
  (when (string= system-type "gnu/linux")
    (setq browse-url-browser-function 'browse-url-firefox)
    (let ((mu4epath
            (concat
              (f-dirname
                (file-truename
                  (executable-find "mu")))
              "/../share/emacs/site-lisp/mu4e")))
      (when (and
              (string-prefix-p "/nix/store/" mu4epath)
              (file-directory-p mu4epath))
        (add-to-list 'load-path mu4epath)))

  ;;; Set up some common mu4e variables
    (setq mu4e-maildir "~/.mail"
      mu4e-trash-folder "/Trash"
      mu4e-refile-folder "/Archive"
      mu4e-get-mail-command "mbsync -a"
      mu4e-update-interval nil
      mu4e-compose-signature-auto-include nil
      mu4e-view-show-images t
      mu4e-view-show-addresses t)

  ;;; Mail directory shortcuts
    (setq mu4e-maildir-shortcuts
      '(("/gmail/INBOX" . ?g)
         ("/college/INBOX" . ?c)))

  ;;; Bookmarks
    (setq mu4e-bookmarks
      `(("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
         ("date:today..now" "Today's messages" ?t)
         ("date:7d..now" "Last 7 days" ?w)
         ("mime:image/*" "Messages with images" ?p)
         (,(mapconcat 'identity
             (mapcar
               (lambda (maildir)
                 (concat "maildir:" (car maildir)))
               mu4e-maildir-shortcuts) " OR ")
           "All inboxes" ?i)))

    (setq mu4e-contexts
      `( ,(make-mu4e-context
            :name "gmail"
            :enter-func (lambda () (mu4e-message "Switch to the gmail context"))
            ;; leave-func not defined
            :match-func (lambda (msg)
                          (when msg
                            (mu4e-message-contact-field-matches msg
                              :to "thomas.o.hartmann@gmail.com")))
            :vars '(  ( user-mail-address      . "thomas.o.hartmann@gmail.com")
                     ( user-full-name     . "Thomas Hartmann")
                     ( mu4e-compose-signature .
                       (concat
                         "Cheers.\n"
                         "Thomas Hartmann\n"))))
         ,(make-mu4e-context
            :name "thomashartmann.dev"
            :enter-func (lambda () (mu4e-message "Switch to the .dev context"))
            ;; leave-fun not defined
            :match-func (lambda (msg)
                          (when msg
                            (mu4e-message-contact-field-matches msg
                              :to "contact@thomashartmann.dev")))
            :vars '(  ( user-mail-address      . "contact@thomashartmann.dev")
                     ( user-full-name     . "Thomas Hartmann")
                     ( mu4e-compose-signature .
                       (concat
                         "Thanks.\n"
                         "Thomas Hartmann\n"))))))
    (message "%s" "Configured mu4e."))
  ;;----------------------------------------------------------------------------
  ;; end email setup
  ;;----------------------------------------------------------------------------
  (message "%s" "Configuring key remapping.")
  ;; key translation
  (define-key key-translation-map (kbd "<S-return>") (kbd "<S-return>"))
  ;; (global-set-key (kbd "C-h")
  ;;   'backward-delete-char-untabify)
  (evil-define-key 'normal
    global-map
    (kbd "gs")
    'transpose-chars)
  ;; Let M-up and -down move the current line linewise
  (define-key global-map (kbd "M-<up>") 'move-line-up)
  (define-key global-map (kbd "M-<down>") 'move-line-down)
  ;; Let M-up and -down move selections linewise
  (define-key evil-visual-state-map (kbd "M-<down>") (concat ":m '>+1" (kbd "RET") "gv=gv"))
  (define-key evil-visual-state-map (kbd "M-<up>") (concat ":m '<-2" (kbd "RET") "gv=gv"))
  (with-eval-after-load 'helm-buffers
    ;; (define-key helm-buffer-map (kbd "C-h") #'backward-delete-char-untabify)
    (define-key helm-buffer-map (kbd "C-d") #'helm-buffer-run-kill-buffers))
  ;; evil
  ;; text objects
  (defmacro define-and-bind-text-object (key start-regex end-regex)
    (let ((inner-name (make-symbol "inner-line"))
           (outer-name (make-symbol "outer-line")))
      `(progn
         (evil-define-text-object ,inner-name
           (count &optional beg end type)
           (evil-select-paren ,start-regex ,end-regex
             beg end type count nil))
         (evil-define-text-object ,outer-name
           (count &optional beg end type)
           (evil-select-paren ,start-regex ,end-regex
             beg end type count t))
         (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
         (define-key evil-outer-text-objects-map ,key (quote ,outer-name)))))
  ;; create "il"/"al" (inside/around) line text objects:
  (define-and-bind-text-object "l" "^\\s-*"
    "\\s-*$")

  (defun smarter-move-beginning-of-line (arg)
    "Move point back to indentation of beginning of line.

    Move point to the first non-whitespace character on this line.
    If point is already there, move to the beginning of the line.
    Effectively toggle between the first non-whitespace character and
    the beginning of the line.

    If ARG is not nil or 1, move forward ARG - 1 lines first.  If
    point reaches the beginning or end of the buffer, stop there."
    (interactive "^p")
    (setq arg (or arg 1))

    ;; Move lines first
    (when (/= arg 1)
      (let ((line-move-visual nil))
        (forward-line (1- arg))))

    (let ((orig-point (point)))
      (back-to-indentation)
      (when (= orig-point (point))
        (move-beginning-of-line 1))))

  ;; make kill and movements in visual line mode still act on logical lines
  (define-key visual-line-mode-map [remap kill-line] nil)
  (define-key visual-line-mode-map [remap move-end-of-line] nil)
  (define-key visual-line-mode-map [remap move-beginning-of-line] nil)

  ;; make kill and movements in org mode act on logical lines
  (with-eval-after-load 'org
    (define-key org-mode-map [remap kill-line] nil)
    (define-key org-mode-map [remap move-end-of-line] nil)
    (define-key org-mode-map [remap move-beginning-of-line] nil))

  (defun isearch-vim-style-exit ()
    "Move point to the start of the matched string, regardless
    of search direction."
    (interactive)
    (when (eq isearch-forward t)
      (goto-char isearch-other-end))
    (isearch-exit))

  (defun isearch-vim-style-kill ()
    "Kill up to the search match when searching forward. When
  searching backward, kill to the beginning of the match."
    (interactive)
    (isearch-vim-style-exit)
    (call-interactively 'kill-region))

  (defun isearch-vim-style-copy ()
    "Copy up to the search match when searching forward. When
    searching backward, copy to the start of the search match."
    (interactive)
    (isearch-vim-style-exit)
    (call-interactively 'kill-ring-save)
    (exchange-point-and-mark))

  (define-key isearch-mode-map (kbd "<C-return>") 'isearch-vim-style-exit)
  (define-key isearch-mode-map (kbd "<M-return>") 'isearch-vim-style-kill)
  (define-key isearch-mode-map (kbd "<C-M-return>") 'isearch-vim-style-copy)

  (define-key global-map [remap move-beginning-of-line] 'smarter-move-beginning-of-line)
  (evil-define-key 'hybrid global-map (kbd "C-a") 'smarter-move-beginning-of-line)

  (setq delete-active-region 'kill)

  (evil-define-key '(hybrid insert) global-map (kbd "C-@") 'undo-tree-undo)

  (evil-leader/set-key "/" 'spacemacs/helm-project-do-rg)
  (evil-define-key 'visual evil-surround-mode-map
    "S" 'evil-surround-region)
  (evil-define-key 'normal
    global-map
    (kbd "C-<tab>")
    'next-buffer)
  (evil-define-key 'normal
    global-map
    (kbd "C-S-<tab>")
    'previous-buffer)
  (evil-define-key '(normal visual)
    global-map
    (kbd "j")
    'evil-next-visual-line)
  (evil-define-key '(normal visual)
    global-map
    (kbd "<down>")
    'evil-next-visual-line)
  (evil-define-key '(normal visual)
    global-map
    (kbd "k")
    'evil-previous-visual-line)
  (evil-define-key '(normal visual)
    global-map
    (kbd "<up>")
    'evil-previous-visual-line)
  (evil-define-key '(normal visual)
    global-map
    (kbd "C-h")
    'help-command)
  (define-key evil-outer-text-objects-map "e"
    'evil-inner-buffer)
  (define-key evil-inner-text-objects-map "e"
    'evil-inner-buffer)
  (define-key evil-normal-state-map (kbd "RET") 'spacemacs/evil-insert-line-below)
  (define-key evil-normal-state-map (kbd "<S-return>") 'spacemacs/evil-insert-line-above)
  (define-key evil-hybrid-state-map (kbd "TAB") #'company-indent-or-complete-common)
  (setq case-fold-search t)
  (setq evil-v$-gets-eol nil)
  ;; stop `v$' from selecting newlines
  (evil-define-motion evil-end-of-line
    (count)
    "Move the cursor to the end of the current line.
If COUNT is given, move COUNT - 1 lines downward first."
    :type inclusive
    (move-end-of-line count)
    (when evil-track-eol
      (setq temporary-goal-column most-positive-fixnum
        this-command 'next-line))
    (unless (and (evil-visual-state-p)
              evil-v$-gets-eol)
      (evil-adjust-cursor)
      (when (eolp)
        ;; prevent "c$" and "d$" from deleting blank lines
        (setq evil-this-type 'exclusive))))
  (spacemacs/set-leader-keys "," 'ace-delete-window
    "-" 'split-window-below-and-focus "." 'ace-window
    "\\" 'split-window-right-and-focus "|" 'split-window-right-and-focus
    "gc" 'magit-commit
    "gd" 'magit-diff-popup "gp" 'magit-push "gBb"
    'magit-checkout "gBc" 'magit-branch-and-checkout
    "gR" 'magit-file-checkout "gF" 'magit-pull
    "jt" 'avy-goto-char-timer "xc" 'count-words)
  (global-company-mode)
  (with-eval-after-load 'company
    (add-hook 'company-mode-hook
      (lambda ()
        (add-to-list 'company-backends 'company-capf)))
    (company-flx-mode +1)
    (setq company-emoji-insert-unicode t)
    ;; (define-key company-active-map (kbd "C-w") 'evil-delete-backward-word)
    ;; (define-key company-active-map (kbd "C-h") 'evil-delete-backward-char)
    (define-key company-active-map [remap indent-for-tab-command] #'company-indent-or-complete-common)
    (define-key company-active-map (kbd "C-d") 'company-show-doc-buffer)
    ;; (define-key company-active-map (kbd "C-n") 'company-select-next)
    ;; (define-key company-active-map (kbd "C-p") 'company-select-previous)
    (define-key company-active-map (kbd "<return>") nil))

  ;; (define-key company-quickhelp-mode-map (kbd "C-n") 'company-select-next)
  ;; (define-key company-quickhelp-mode-map (kbd "C-p") 'company-select-previous))

  ;; Make C-/ expand yasnippet if available, else go into company
  ;; Must unbind undo tree first
  (with-eval-after-load 'undo-tree
    (define-key undo-tree-map (kbd "C-/") nil))
  (defun insert-date ()
    (interactive)
    (insert (format-time-string "%F")))
  (spacemacs/set-leader-keys "id" 'insert-date)
  (defun insert-iso-timestamp ()
    (interactive)
    (insert (format-time-string "%FT%T%:z")))
  (spacemacs/set-leader-keys "it" 'insert-iso-timestamp)
  (defun insert-shrug ()
    (interactive)
    (insert "¯\\_(ツ)_/¯"))
  (defun Ø () (interactive) (insert "Ø"))
  (defun ø () (interactive) (insert "ø"))
  (defun Æ () (interactive) (insert "Æ"))
  (defun æ () (interactive) (insert "æ"))
  (defun Å () (interactive) (insert "Å"))
  (defun å () (interactive) (insert "å"))
  (spacemacs/set-leader-keys "i?" 'insert-shrug)
  (spacemacs/set-leader-keys "inO" 'Ø)
  (spacemacs/set-leader-keys "ino" 'ø)
  (spacemacs/set-leader-keys "inE" 'Æ)
  (spacemacs/set-leader-keys "ine" 'æ)
  (spacemacs/set-leader-keys "inA" 'Å)
  (spacemacs/set-leader-keys "ina" 'å)
  (spacemacs/set-leader-keys "wn" 'other-frame)
  (spacemacs/set-leader-keys "wp" (lambda () (interactive) (other-frame -1)))
  ;; (defun nir-yasnippet-expand-or-complete ()
  ;;   (interactive)
  ;;   (unless (call-interactively 'yas-expand) (call-interactively 'company-yasnippet)))
  ;; Must bind in global map, else undo tree stops loading
  (with-eval-after-load 'yasnippet
    (define-key global-map (kbd "C-/") 'yas-expand))
  (spacemacs/toggle-indent-guide-globally-on)

  (use-package mixed-pitch
    :hook
    ;; for use in all text modes
    (org-mode . mixed-pitch-mode)
    (markdown-mode . mixed-pitch-mode)
    (LaTeX-mode . mixed-pitch-mode))

  (message "%s" "Configured key remapping.")
  ;;----------------------------------------------------------------------------
  ;; Ranger setup
  ;;----------------------------------------------------------------------------
  (with-eval-after-load 'ranger
    (setq
      ranger-override-dired-mode t
      ranger-cleanup-eagerly t
      ranger-show-hidden t
      ranger-parent-depth 0
      ranger-show-literal t
      ranger-dont-show-binary t
      ranger-max-preview-size 10))
  (message "%s" "Configured ranger.")
  ;;----------------------------------------------------------------------------
  ;; end Ranger setup
  ;;----------------------------------------------------------------------------

  ;;----------------------------------------------------------------------------
  ;; Eshell setup
  ;;----------------------------------------------------------------------------
  (add-hook 'eshell-mode-hook
    'direnv-mode)

  ;; (with-eval-after-load 'eshell)
  ;; (add-hook 'eshell-mode-hook
  ;;   (lambda ()
  ;;     (setenv "TERM" "xterm-256color")))
  ;; (add-hook 'eshell-before-prompt-hook (setq xterm-color-preserve-properties t))
  ;; (setq eshell-output-filter-functions
  ;;   (remove 'eshell-handle-ansi-color eshell-output-filter-functions))
  ;; (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)

  (message "%s" "Configured eshell.")

  ;;----------------------------------------------------------------------------
  ;; end Eshell setup
  ;;----------------------------------------------------------------------------


  ;;----------------------------------------------------------------------------
  ;; Org setup
  ;;----------------------------------------------------------------------------

  (defun my-create-log-msg (text)
    "Create a log message with a timestamp."
    (format "[%s] %s" (format-time-string "%T:%3N") text))

  (defun my-print-log-msg (text)
    (message "%s" (my-create-log-msg text)))

  (defun log-config-start (what)
    "Log the start of a configuration."
    (my-print-log-msg (format "Configuring %s" what)))

  (defun log-config-end (what)
    "Log the end of a configuration."
    (my-print-log-msg (format "Finished configuring %s" what)))

  ;; TODO: find a way to make this work without lambdas in the calling function
  (defun configure (what config)
    "Configure something and wrap it in logs."
    (log-config-start what)
    (funcall-interactively config)
    (log-config-end what))

  (configure "Org mode"
    '(lambda () (org-babel-load-file
                  (expand-file-name "config.org"
                    user-emacs-directory))))

  ;;----------------------------------------------------------------------------
  ;; end Org setup
  ;;----------------------------------------------------------------------------


  ;;----------------------------------------------------------------------------
  ;; LaTeX setup
  ;;----------------------------------------------------------------------------

  ;; disable automatic hard wrapping of lines
  (add-hook 'TeX-mode-hook (spacemacs/toggle-auto-fill-mode-off))

  ;; make , i i not cut off the last letter of a list item when inserting a new one.
  (defun spacemacs/latex-insert-item ()
    (interactive "*")
    (if (evil-normal-state-p)
      (progn (evil-open-below 1)
        (beginning-of-line)
        (call-interactively 'LaTeX-insert-item))
      (call-interactively 'LaTeX-insert-item)))
  (spacemacs/set-leader-keys-for-major-mode 'latex-mode
    "i" 'spacemacs/latex-insert-item)
  (message "%s" "Configured Latex.")

  ;;----------------------------------------------------------------------------
  ;; end LaTeX setup
  ;;----------------------------------------------------------------------------

  (defun on-after-init()
    (unless (display-graphic-p (selected-frame))
      (set-face-background 'default
        "unspecified-bg"
        (selected-frame))))
  (add-hook 'window-setup-hook 'on-after-init)
  (defmacro save-column (&rest body)
    `(let ((column (current-column)))
       (unwind-protect
         (progn ,@body)
         (move-to-column column))))
  (put 'save-column 'lisp-indent-function 0)
  (defun move-line-up ()
    (interactive)
    (save-column
      (transpose-lines 1)
      (forward-line -2)))
  (defun move-line-down ()
    (interactive)
    (save-column
      (forward-line 1)
      (transpose-lines 1)
      (forward-line -1)))
  (when (string= system-type "darwin")
    ;; mac specific setup
    (setq dired-use-ls-dired nil)
    (setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin"))
    (add-to-list 'configuration-layer-elpa-archives '("melpa-stable" . "stable.melpa.org/packages/"))
    (add-to-list 'package-pinned-packages '(spaceline . "melpa-stable"))
    (add-to-list 'package-pinned-packages '(spaceline-all-the-icons . "melpa-stable"))
    (add-to-list 'package-pinned-packages '(all-the-icons . "melpa-stable"))
    (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
    (setenv "PATH" (concat (getenv "PATH") ":/Library/Frameworks/Mono.framework/Commands:/usr/local/share/dotnet:~/.dotnet/tools"))
    (setq exec-path (append exec-path '("/Library/Frameworks/Mono.framework/Commands" "/usr/local/share/dotnet" "~/.dotnet/tools"))))


  (when (string= system-type "windows-nt")
    ;; windows specific setup
    (setq projectile-git-submodule-command nil))



  (when (string= system-type "gnu/linux")
    ;; linux specific setup
    ;; activate emoji
    (setq browse-url-browser-function 'browse-url-firefox)
    (set-fontset-font t 'unicode "Symbola" nil 'prepend)
    ;; set fonts correctly
    (setq-default dotspacemacs-default-font '(("Hack" :size 28)
                                               ("Input Mono" :size 28)
                                               ("Dank Mono" :size 28
                                                 :weight normal
                                                 :width normal
                                                 :powerline-scale 1.1))))

  (custom-set-faces '(company-tooltip-common ((t (:inherit company-tooltip :weight bold
                                                   :underline nil))))
    '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold
                                             :underline nil)))))

  ;;----------------------------------------------------------------------------
  ;; .NET setup
  ;;----------------------------------------------------------------------------
  (add-to-list 'auto-mode-alist '("\\.*sproj\\'" . html-mode))
  ;;----------------------------------------------------------------------------
  ;; C# / Omnisharp setup
  ;;----------------------------------------------------------------------------
  (message "%s" "Configuring C#/omnisharp.")
  (spacemacs/set-leader-keys-for-major-mode
    'csharp-mode "=" 'omnisharp-code-format-entire-file)
  (spacemacs/set-leader-keys-for-major-mode
    'csharp-mode "c r" 'recompile)
  (eval-after-load 'company
    '(add-to-list 'company-backends #'company-omnisharp))
  (defun my-csharp-mode-hook ()
    (add-hook 'before-save-hook 'omnisharp-code-format-entire-file t 'local)
    (omnisharp-mode)
    (company-mode)
    (flycheck-mode)
    (dotnet-mode)
    (lsp-ui-mode)
    (c-set-offset 'arglist-intro '+)
    (c-set-offset 'arglist-cont-nonempty '+)
    (c-set-offset 'topmost-intro-cont '+)
    (c-set-offset 'arglist-cont-nonempty '+)
    (c-set-offset 'arglist-close 0)
    (setq electric-pair-local-mode nil))
  (add-hook 'csharp-mode-hook
    'my-csharp-mode-hook)
  (add-to-list 'auto-mode-alist
    '("\\.cshtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist
    '("\\.cshtml$" . web-mode))
  (message "%s" "Configured C#/omnisharp.")
  ;; smartparens
  ;; dotspacemacs-smartparens-strict-mode t
  (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
  ;; editorconfig
  (editorconfig-mode t)

  ;;----------------------------------------------------------------------------
  ;; F#
  ;;----------------------------------------------------------------------------
  ;; fantomas
  (message "%s" "Configuring F#.")
  (defun fsharp-fantomas-format-region (start end)
    (interactive "r")
    (let ((source (shell-quote-argument (buffer-substring-no-properties start end)))
           (ok-buffer "*fantomas*")
           (error-buffer "*fantomas-errors*"))
      (save-window-excursion
        (shell-command-on-region
          start end (format "fantomas --force --preserveEOL --reorderOpenDeclaration --pageWidth 80 --stdin %s --stdout" source)
          ok-buffer nil error-buffer)
        (if (get-buffer error-buffer)
          (progn
            (kill-buffer error-buffer)
            (message "Fantomas: Can't format region."))
          (delete-region start end)
          (insert (with-current-buffer ok-buffer
                    (s-chomp (buffer-string))))
          (delete-trailing-whitespace)
          (when (get-buffer error-buffer)
            (kill-buffer error-buffer))
          (kill-buffer ok-buffer)
          (message "Fantomas: Region formatted.")))))

  (defun fsharp-fantomas-format-defun ()
    (interactive)
    (let ((origin (point))
           (start) (end))
      (fsharp-beginning-of-block)
      (setq start (point))
      (fsharp-end-of-block)
      ;; skip whitespace, empty lines, comments
      (while (and (not (= (line-number-at-pos) 1))
               (s-matches? "^\n$\\|^//\\|^(\\*" (thing-at-point 'line)))
        (forward-line -1))
      (move-end-of-line 1)
      (setq end (point))
      (fsharp-fantomas-format-region start end)
      (goto-char origin)))

  (defun fsharp-fantomas-format-buffer ()
    (interactive)
    (let ((origin (point)))
      (fsharp-fantomas-format-region (point-min) (point-max))
      (goto-char origin)))

  (add-hook 'fsharp-mode-hook
    (lambda ()
      (add-hook 'before-save-hook 'fsharp-fantomas-format-buffer t 'local)
      (lsp-mode)
      (dotnet-mode)))
  (message "%s" "Finished configuring F#.")
  ;;----------------------------------------------------------------------------
  ;; end F#
  ;;----------------------------------------------------------------------------

  ;;----------------------------------------------------------------------------
  ;; JS
  ;;----------------------------------------------------------------------------
  (message "%s" "Configuring JS.")

  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'javascript-eslint 'js2-mode)
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    (add-to-list 'flycheck-checkers 'lsp-ui))
  (add-hook 'js2-mode-hook 'eslintd-fix-mode)
  (add-hook 'web-mode-hook 'eslintd-fix-mode)
  ;; json
  (setq-default js-indent-level 2)
  (setq json-reformat:indent-width 2)
  ;; web-mode
  (setq web-mode-script-padding 0)
  (setq web-mode-style-padding 0)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (require 'web-mode)
  (add-hook 'web-mode-hook #'turn-on-smartparens-mode
    t)
  (message "%s" "Configured JS.")

  ;;----------------------------------------------------------------------------
  ;; Markdown setup
  ;;----------------------------------------------------------------------------
  (spacemacs/set-leader-keys-for-major-mode
    'markdown-mode "ih" 'markdown-insert-header)
  ;;----------------------------------------------------------------------------
  ;; end Markdown setup
  ;;----------------------------------------------------------------------------
  ;;----------------------------------------------------------------------------
  ;; SCSS setup
  ;;----------------------------------------------------------------------------
  (add-hook 'scss-mode-hook 'prettier-js-mode)
  (spacemacs/set-leader-keys-for-major-mode
    'scss-mode "=" 'prettier-js)
  (add-hook 'css-mode-hook 'prettier-js-mode)
  (spacemacs/set-leader-keys-for-major-mode
    'css-mode "=" 'prettier-js)
  (add-hook 'json-mode-hook 'prettier-js-mode)
  (spacemacs/set-leader-keys-for-major-mode
    'json-mode "=" 'prettier-js)
  (spacemacs/set-leader-keys-for-major-mode
    'web-mode "=" 'prettier-js)
  (message "%s" "Configured prettier modes.")
  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'scss-stylelint 'scss-mode)
    (flycheck-add-mode 'javascript-eslint 'js2-mode)
    (flycheck-add-mode 'javascript-eslint 'web-mode))
  ;;----------------------------------------------------------------------------
  ;; JS setup
  ;;----------------------------------------------------------------------------

  (message "%s" "Configuring eslint modes.")
  ;; (setq-default flycheck-disabled-checkers (append flycheck-disabled-javascript
  ;;                                                  '(checkers-jshint)))
  (add-hook 'js2-mode-hook 'eslintd-fix-mode)
  (add-hook 'web-mode-hook 'eslintd-fix-mode)
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'web-mode-hook 'prettier-js-mode)
  (add-hook 'typescript-tsx-mode 'prettier-js-mode)
  (add-hook 'markdown-mode-hook 'prettier-js-mode)
  ;; web-mode
  (message "%s" "Configuring web mode.")
  (with-eval-after-load 'web-mode
    (add-hook 'web-mode-hook #'turn-on-smartparens-mode
      t))
  (with-eval-after-load 'lsp-ui
    (require 'lsp-ui-flycheck)
    (add-hook 'lsp-mode-hook 'lsp-ui-mode))
  (require 'company-lsp)
  (push 'company-lsp company-backends)
  (message "%s" "Configured web stuff.")

  ;;----------------------------------------------------------------------------
  ;; Haskell setup
  ;;----------------------------------------------------------------------------
  ;; insert space after λ> in repl
  (message "%s" "Configuring haskell.")
  (when (configuration-layer/package-usedp 'haskell)
    (add-hook 'haskell-interactive-mode-hook
      (lambda ()
        (setq-local evil-move-cursor-back nil))))
  ;; start repl in insert mode
  (when (configuration-layer/package-usedp 'haskell)
    (defadvice haskell-interactive-switch
      (after spacemacs/haskell-interactive-switch-advice
        activate)
      (when (eq dotspacemacs-editing-style 'vim)
        (call-interactively 'evil-insert))))
  ;; make `=` available for formatting
  (spacemacs/set-leader-keys-for-major-mode
    'haskell-mode "=" 'hindent-reformat-buffer)
  (setq hindent-reformat-buffer-on-save t)
  (add-hook 'haskell-mode-hook 'hindent-mode)
  ;; reset indentation after a blank line
  (defun haskell-indentation-advice ()
    (when (and (< 1 (line-number-at-pos))
            (save-excursion
              (forward-line -1)
              (string= ""
                (s-trim (buffer-substring (line-beginning-position)
                          (line-end-position))))))
      (delete-region (line-beginning-position)
        (point))))
  (advice-add 'haskell-indentation-newline-and-indent
    :after 'haskell-indentation-advice)
  (with-eval-after-load "haskell-mode"
    ;; This changes the evil "O" and "o" keys for haskell-mode to make sure that
    ;; indentation is done correctly. See
    ;; https://github.com/haskell/haskell-mode/issues/1265#issuecomment-252492026.
    (defun haskell-evil-open-above ()
      (interactive)
      (evil-digit-argument-or-evil-beginning-of-line)
      (haskell-indentation-newline-and-indent)
      (evil-previous-line)
      (haskell-indentation-indent-line)
      (evil-append-line nil))
    (defun haskell-evil-open-below ()
      (interactive)
      (evil-append-line nil)
      (haskell-indentation-newline-and-indent))
    (evil-define-key 'normal haskell-mode-map
      "o" 'haskell-evil-open-below "O" 'haskell-evil-open-above))
  (message "%s" "Configured haskell.")

  ;;----------------------------------------------------------------------------
  ;; macOS setup
  ;;----------------------------------------------------------------------------
  (when (eq system-type 'darwin)
    (message "Configuring for macOS")
    (setq mac-option-modifier nil
      mac-command-modifier 'meta)
    (setq lsp-ui-doc-use-childframe nil)
    (message "macOS configuration complete"))
  ;;----------------------------------------------------------------------------
  ;; end  macOS setup
  ;;----------------------------------------------------------------------------
  (with-eval-after-load 'pdf-outline-buffer-mode (setq variable-pitch-mode t))


  ;;----------------------------------------------------------------------------
  ;; git setup
  ;;----------------------------------------------------------------------------
  (message "Setting up git")

  ;; this may seem a strange place to require window-purpose, but there are some
  ;; issues with circular dependencies causing emacs to crawl to a halt.
  ;; See https://github.com/bmag/emacs-purpose/issues/158#issuecomment-547293349
  ;; for more information

  (require 'window-purpose)
  (use-package forge
    :after magit
    :config (add-to-list 'forge-alist '("gitlab.intility.no" "gitlab.intility.no/api/v4" "gitlab.intility.no" forge-gitlab-repository))
    (define-key magit-mode-map (kbd "C-c M-w") 'forge-copy-url-at-point-as-kill))
  (message "Git config complete.")
  ;;----------------------------------------------------------------------------
  ;; end git setup
  ;;----------------------------------------------------------------------------
  (message "%s" "Finished user-config."))



;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  '(package-selected-packages (quote (zoom-window magit-p4 p4 company-flx editorconfig
                                       js-format gitter slime-company slime common-lisp-snippets
                                       web-beautify livid-mode skewer-mode simple-httpd
                                       json-mode json-snatcher json-reformat js2-refactor
                                       multiple-cursors js2-mode js-doc company-tern
                                       dash-functional tern coffee-mode memoize all-the-icons
                                       company-quickhelp git-gutter-fringe+ git-gutter-fringe
                                       fringe-helper git-gutter+ git-gutter diff-hl
                                       company-web web-completion-data web-mode tagedit
                                       slim-mode scss-mode sass-mode pug-mode less-css-mode
                                       helm-css-scss haml-mode emmet-mode xterm-color
                                       shell-pop multi-term mmm-mode markdown-toc
                                       markdown-mode gh-md flyspell-correct-helm
                                       flyspell-correct flycheck-rust flycheck-pos-tip
                                       flycheck-elm flycheck eshell-z eshell-prompt-extras
                                       esh-help auto-dictionary helm-company helm-c-yasnippet
                                       fuzzy company-statistics company auto-yasnippet
                                       yasnippet ac-ispell auto-complete evil-avy
                                       atom-one-dark-theme toml-mode racer pos-tip
                                       cargo rust-mode elm-mode smeargle orgit org-projectile
                                       org-category-capture org-present org-pomodoro
                                       alert log4e gntp org-mime org-download magit-gitflow
                                       htmlize helm-gitignore gnuplot gitignore-mode
                                       gitconfig-mode gitattributes-mode git-timemachine
                                       git-messenger git-link evil-magit magit magit-popup
                                       git-commit ghub with-editor ws-butler winum
                                       which-key volatile-highlights vi-tilde-fringe
                                       uuidgen use-package toc-org spaceline restart-emacs
                                       request rainbow-delimiters popwin persp-mode
                                       pcre2el paradox org-plus-contrib org-bullets
                                       open-junk-file neotree move-text macrostep
                                       lorem-ipsum linum-relative link-hint indent-guide
                                       hungry-delete hl-todo highlight-parentheses
                                       highlight-numbers highlight-indentation helm-themes
                                       helm-swoop helm-projectile helm-mode-manager
                                       helm-make helm-flx helm-descbinds helm-ag
                                       google-translate golden-ratio flx-ido fill-column-indicator
                                       fancy-battery eyebrowse expand-region exec-path-from-shell
                                       evil-visualstar evil-visual-mark-mode evil-unimpaired
                                       evil-tutor evil-surround evil-search-highlight-persist
                                       evil-numbers evil-nerd-commenter evil-mc evil-matchit
                                       evil-lisp-state evil-indent-plus evil-iedit-state
                                       evil-exchange evil-escape evil-ediff evil-args
                                       evil-anzu eval-sexp-fu elisp-slime-nav dumb-jump
                                       diminish define-word column-enforce-mode clean-aindent-mode
                                       auto-highlight-symbol auto-compile aggressive-indent
                                       adaptive-wrap ace-window ace-link ace-jump-helm-line))))
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
    (dap-mode bui company-plsense zoom-window magit-p4 p4 company-flx editorconfig js-format gitter slime-company slime common-lisp-snippets web-beautify livid-mode skewer-mode simple-httpd json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc company-tern dash-functional tern coffee-mode memoize all-the-icons company-quickhelp git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter diff-hl company-web web-completion-data web-mode tagedit slim-mode scss-mode sass-mode pug-mode less-css-mode helm-css-scss haml-mode emmet-mode xterm-color shell-pop multi-term mmm-mode markdown-toc markdown-mode gh-md flyspell-correct-helm flyspell-correct flycheck-rust flycheck-pos-tip flycheck-elm flycheck eshell-z eshell-prompt-extras esh-help auto-dictionary helm-company helm-c-yasnippet fuzzy company-statistics company auto-yasnippet yasnippet ac-ispell auto-complete evil-avy atom-one-dark-theme toml-mode racer pos-tip cargo rust-mode elm-mode smeargle orgit org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-mime org-download magit-gitflow htmlize helm-gitignore gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link evil-magit magit magit-popup git-commit ghub with-editor ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint indent-guide hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu elisp-slime-nav dumb-jump diminish define-word column-enforce-mode clean-aindent-mode auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
)
