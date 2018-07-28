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

# name of repo, private, user
alias newrepo='github_create_and_clone $1 $2 $3'
alias newrepoonline='github_createrepo $1 $2 $3'
# name of repo, user
alias delrepo='github_deleterepo $1 $2'
# user, token
alias repos='github_listrepos $1 $2'
alias clone='github_clone_repo $1 $2'

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
alias py3="/usr/local/bin/python3.6"
alias py="python"

# misc
alias lsa="ls -lah"
alias rbo="rm -rf bin obj" #removes bin and obj folders. Useful for cleaning a VS project
alias pd="pandoc"
alias mellon="alohomora" # speak friend and enter
alias path="echo \"${PATH//:/\n}\""

# dirs
alias md='mkdir -p'
alias rd="rmdir"
alias -- -="cd -"

# settings etc
alias conf="em ~/.settings/"
alias sets="cd ~/.settings/ && l"
alias sc="cd ~/.settings/custom && ls"

# navigation
alias desk="cd $HOME/Desktop && l"
alias docs="cd ~/Documents && l"
alias down="cd ~/Downloads && l"

alias droid="adb devices"