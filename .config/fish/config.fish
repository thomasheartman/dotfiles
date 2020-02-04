source ~/.aliases/aliases.fish

switch (uname)
    case Darwin
        set -gx EDITOR "vim"
        set -U fish_user_paths $HOME/.cargo/bin $HOME/.yarn/bin ./node_modules $HOME/.local/bin $HOME/.dotnet/tools
    case '*'
        set -gx EDITOR "emacsclient -t"
        bind \b 'backward-kill-word'
end
set -gx VISUAL $EDITOR

# explicitly set the lang to one that supports UTF-8 for TMUX unicode support
set -x LANG en_US.UTF-8

# disable greeting
set fish_greeting

function fish_user_key_bindings
  fish_default_key_bindings
  bind \cN accept-autosuggestion
  bind \e\[4~ 'kill-word'
end

set -g fish_key_bindings fish_user_key_bindings

# ssh
setenv SSH_ENV $HOME/.ssh/environment

if type -q direnv
    direnv hook fish | source
end

# fix fish in emacs
function fish_title
  true
end
