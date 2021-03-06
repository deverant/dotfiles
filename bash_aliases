alias ec='emacsclient -n -a ""'

alias urlencode='python -c "import sys, urllib; print urllib.quote_plus(sys.argv[1])"'
alias urldecode='python -c "import sys, urllib; print urllib.unquote_plus(sys.argv[1])"'

alias ..='cd ..'
alias ..2='cd ../..'
alias ..3='cd ../../..'
alias ..4='cd ../../../..'
alias ..5='cd ../../../../..'

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# bash history grep from all the files
hist() {
    find ~/.history -type f|sort|xargs awk '{ print FILENAME ":" $0 }'
    history
}
hgrep() {
    hist | grep -a "$@"
}


# open emacs right here right now
e() {
    emacsclient -c -nw $@
}

# open emacs in tmux split window
em() {
    tmux split-window -h 'emacsclient -c -nw '$@''
}
