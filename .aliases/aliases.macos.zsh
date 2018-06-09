case $OSTYPE in darwin*)

alias fb="firebase"
alias mail="open -a mail"

# misc
alias gconf="em ~/.gitconfig"
alias reindex="sudo mdutil -i on /" #rebuild spotlight index
alias empty="sudo rm -rf ~/.Trash/*" #deletes all objects in trash can
alias emoty="empty" #because I tend to misspell things :/
alias jbs="cd ~/JetBrainsSettings/" #for local version of JetBrainsSettings repo

alias vi="vim" # to get the good kind of vim

# Westerdals navigation
alias -g w1="~/MEGA/Westerdals/Spillprogrammering/2015–2016"
alias -g w2="~/MEGA/Westerdals/Spillprogrammering/2016–2017"
alias -g w3="~/MEGA/Westerdals/Spillprogrammering/2017-2018"

# general navigation
# alias desk="cd ~/Desktop/ && ls -1"
alias -g desk="~/Desktop/"

alias @="open"

alias dl="diskutil list"
alias disks="diskutil list" #double aliased in case dl gets hijacked

alias dot="/usr/bin/git --git-dir=$HOME/dotfiles/ --work-tree=$HOME"

;; esac
