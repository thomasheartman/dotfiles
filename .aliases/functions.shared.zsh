# functions for sleeping etc
function rest() { TIME=${1:-now}; sudo shutdown -s $TIME; }

function kill() {
    TIME=${1:-now}
    case $OSTYPE in
        darwin*)
        halt
        ;;
        linux*)
        shutdown $TIME
        ;;
    esac
}

# unlocking
function alohomora() { sudo chmod -R ${2:-777} ${1:-./**/*}; }
# locking
function colloportus() { alohomora ${1:-./**/*}  ${2:-000}; }

# git commit
function com() {
    git add -A;
    git commit;
}


function google()
{
    if [ $# -eq 0 ];
        then
            open "https://www.google.com/"
    else
        open "https://www.google.com/search?q=$@"
    fi
}

function take() {
  mkdir -p $1
  cd $1
}

function tidy ()
{
    if [ $# -eq 0 ]
    then
        REBASE_TARGET="@{u}"
    elif [ ${#1} -ge 4 ]
    then
        REBASE_TARGET=$1
    else
        REBASE_TARGET="HEAD~$1"
    fi
    git rebase -i $REBASE_TARGET
}
