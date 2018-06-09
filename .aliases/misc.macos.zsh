case $OSTYPE in darwin*)

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

# added by travis gem
[ -f /Users/Thomas/.travis/travis.sh ] && source /Users/Thomas/.travis/travis.sh

;; esac