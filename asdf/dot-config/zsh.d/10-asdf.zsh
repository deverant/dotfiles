if [ -r "${HOME}/.asdf/asdf.sh" ]
then
    source "${HOME}/.asdf/asdf.sh"

    # only look at .go-version
    export ASDF_GOLANG_MOD_VERSION_ENABLED=false

    if [ -r "${HOME}/.asdf/plugins/java/set-java-home.zsh" ]
    then
	source "${HOME}/.asdf/plugins/java/set-java-home.zsh"
    fi

    if [ -r "${HOME}/.asdf/plugins/golang/set-env.zsh" ]
    then
	source "${HOME}/.asdf/plugins/golang/set-env.zsh"
    fi

    # initialise completions with ZSH's compinit
    fpath=(${ASDF_DIR}/completions $fpath)
fi
