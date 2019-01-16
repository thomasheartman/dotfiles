alias vi="vim" # to get the good kind of vim
alias emt="emacsclient -t"
alias dot="git --git-dir=$HOME/dotfiles/ --work-tree=$HOME"
alias windot="/usr/bin/git --git-dir=/mnt/c/Users/thomas/dotfiles/ --work-tree=/mnt/c/Users/thomas"
alias nixdot="git --git-dir=/etc/nixos/git/ --work-tree=/etc/nixos/"

alias p="pijul"

alias sc="systemctl"
alias jc="journalctl"

# git
alias g="git"
alias gfs="git fs"
alias ggf="git gf" #gotta go fast!
alias push="git push"
alias oush="git push"
alias pull="git pull"
alias gf="git fix"
alias stash="git stash"
alias merge="git merge --no-ff"
alias update="git update"
alias tag="git tag -a"

#take all uncommitted and un-staged changes currently in the working directory and add them to the previous commit, amending it before pushing the change up
alias caa="git commit -a --amend -C HEAD"

# yarn
alias ya="yarn"
alias yas="yarn start"
alias yad="yarn add"
alias gad="yarn global add"
alias yrm="yarn remove"
alias yev="ya add --dev"

#standard JS
alias fix="standard --fix"
alias std="standard --verbose | snazzy"

# python
alias py="python"

# misc
alias l="ls -l"
alias la="ls -la"

if type -q lsd
    alias ls="lsd"
end

alias pd="pandoc"
alias mellon="alohomora" # speak friend and enter
alias tx="tmux"
alias txa="tmux attach"

# dirs
alias md='mkdir -p'
alias rd="rmdir"

alias droid="adb devices"

switch (uname)
    case Linux
        alias open="xdg-open"
end

alias refresh="source ~/.config/fish/config.fish"