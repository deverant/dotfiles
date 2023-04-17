# Run emacs daemon if needed
if ! ( emacsclient --eval "(server-running-p)" 1>/dev/null 2>&1 );
then
    emacs --daemon
fi

export EDITOR="emacsclient -nw"

e() {
    emacsclient -a "" -nw "$@"
}

ew() {
    emacsclient -a "" -c -n "$@"
}

magit() {
    local root="$(git rev-parse --show-toplevel 2>/dev/null)"
    if [ "${root}" != "" ]; then
        emacsclient -a "" -nw -e "(progn (magit-status \"${root}\") (delete-other-windows))"
    else
        echo "magit: no git repository found"
        return 0;
    fi
}
