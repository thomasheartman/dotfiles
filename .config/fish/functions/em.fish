function em
    if emacsclient -ne "(if (> (length (frame-list)) 1) 't)" | grep -q t
        emacsclient -nq $argv
    else
        emacsclient -a '' -nqc $argv
    end
end
