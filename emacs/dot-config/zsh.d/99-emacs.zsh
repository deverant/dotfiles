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
	emacsclient \
	    -a "" \
	    -u \
	    -nw \
	    -e "(progn (magit-status \"${root}\") (delete-other-windows))"
    else
	echo "magit: no git repository found"
	return 0;
    fi
}
