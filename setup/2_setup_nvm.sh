#!/bin/bash

DOTFILES_DIR=$(pwd)

# backup ~/.nvm
find $HOME -maxdepth 1 -name .nvm -exec mv {} {}.bak \;

# install nvm
ln -vs $DOTFILES_DIR/.nvm $HOME/.nvm
export NVM_DIR="$HOME/.nvm"

# start nvm
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm

# install node
#nvm install v10.6.3
nvm install --lts

# npm delete prefix
#npm config delete prefix
#npm config set prefix $NVM_DIR/versions/node/v10.6.3

# nvm alias default
#nvm alias default
