source ~/.aliases/aliases.fish

switch (uname)
    case Darwin
        set -gx EDITOR "emacsclient"
        set -U fish_user_paths $HOME/.cargo/bin $HOME/.yarn/bin ./node_modules $HOME/.local/bin $HOME/.dotnet/tools
        [ -f /usr/local/share/autojump/autojump.fish ]; and source /usr/local/share/autojump/autojump.fish
        set DARWIN_NIX_CONFIG "/etc/static/fish/config.fish"
        [ -f $DARWIN_NIX_CONFIG ]; and source $DARWIN_NIX_CONFIG;
        if type -q any-nix-shell
            any-nix-shell fish --info-right | source
        end

    case '*'
        set -gx EDITOR "emacsclient"
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
    set -q argv[1]; and set program "($argv)"
    echo 🐟 (fish_prompt_pwd_dir_length=1 prompt_pwd) $program
end
