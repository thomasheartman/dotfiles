for file in ${0:h}/**/(aliases|functions).*.zsh
do
    source $file
done