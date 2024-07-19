if ! type docker &>/dev/null
then
    return 0
fi

if [ ! -d "${HOME}/.docker/completions/_docker" ]
then
    mkdir -p ${HOME}/.docker/completions
    docker completion zsh > ${HOME}/.docker/completions/_docker
fi

fpath=(${HOME}/.docker/completions \\$fpath)
