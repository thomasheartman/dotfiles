function em
    if type -q emacsclient
        if emacsclient -ne "(if (> (length (frame-list)) 1) 't)" | grep -q t > /dev/null
            emacsclient -nq $argv
        else
            emacsclient -a '' -nqc $argv
        end
    else
        echo "emacsclient isn't available (this fn is defined in `.config/fish/functions/em.fish`)"
    end
end
