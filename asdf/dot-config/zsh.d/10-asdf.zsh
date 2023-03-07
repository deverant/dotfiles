if [ -r "${HOME}/.asdf/asdf.sh" ]
then
    source "${HOME}/.asdf/asdf.sh"

    if [ -r "${HOME}/.asdf/plugins/java/set-java-home.zsh" ]
    then
        source "${HOME}/.asdf/plugins/java/set-java-home.zsh"
    fi

    # initialise completions with ZSH's compinit
    fpath=(${ASDF_DIR}/completions $fpath)
fi
