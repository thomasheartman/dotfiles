source ~/.aliases/aliases.macos.sh
source ~/.aliases/aliases.shared.zsh

set $EDITOR="vim"
set $VISUAL=$EDITOR
set PATH $HOME/.cargo/bin $PATH

function fish_user_key_bindings
  fish_vi_key_bindings
  bind -M insert \cN accept-autosuggestion
end

set -g fish_key_bindings fish_user_key_bindings
