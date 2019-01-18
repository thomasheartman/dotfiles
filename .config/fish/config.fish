source ~/.aliases/aliases.fish

set -gx EDITOR "emacsclient -t"
set -gx VISUAL $EDITOR

# disable greeting
set fish_greeting

if not begin test -e /etc/os-release; and cat /etc/os-release | grep ID=nixos > /dev/null; end
    if not set -q fish_user_paths[1]
        set -U fish_user_paths $HOME/.cargo/bin $HOME/.yarn/bin ./node_modules $HOME/.local/bin
    end
end

function fish_user_key_bindings
  fish_vi_key_bindings
  bind -M insert \cN accept-autosuggestion
end

set -g fish_key_bindings fish_user_key_bindings

# ssh
setenv SSH_ENV $HOME/.ssh/environment