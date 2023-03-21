# Run emacs daemon if needed
if ! ( emacsclient --eval "(server-running-p)" 1>/dev/null 2>&1 );
then
    emacs --daemon
fi

export EDITOR="emacsclient -nw"

e() {
    emacsclient -nw "$@"
}

ew() {
    emacsclient -c -n "$@"
}
