if (( $+commands[keychain] ))
then
    eval `${commands[keychain]} -q --eval --agents ssh --inherit any id_rsa`
fi
