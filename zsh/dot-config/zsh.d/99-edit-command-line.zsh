# Support C-x C-e as a way to edit the command line

autoload -z edit-command-line
zle -N edit-command-line
bindkey "^X^E" edit-command-line
