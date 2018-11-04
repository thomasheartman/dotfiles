source ~/.aliases/aliases.fish

set -gx EDITOR vim
set -gx VISUAL $EDITOR
set PATH $HOME/.cargo/bin $HOME/.yarn/bin ./node_modules $PATH

function fish_user_key_bindings
  fish_vi_key_bindings
  bind -M insert \cN accept-autosuggestion
end

set -g fish_key_bindings fish_user_key_bindings
