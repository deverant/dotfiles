if (( $+commands[keychain] ))
then
    eval `${commands[keychain]} -q --eval --agents ssh --inherit local-once id_rsa`
fi
