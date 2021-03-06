# Setup

## Setup Script

```sh
$ cd dotfiles # cd into dotfiles root directory
$ source ./install.sh
```

## Manual Setup

```sh
# setup vim symlinks
$ ln -vs dotfiles/.vim ~/.vim
$ ln -vs dotfiles/.vimrc ~/.vimrc

# install nvm
$ ln -vs dotfiles/.nvm ~/.nvm
$ export NVM_DIR="$HOME/.nvm"

# start nvm
$ [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
$ [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm

# install node
$ nvm install v10.6.3

# npm delete prefix
$ npm config delete prefix
$ npm config set prefix $NVM_DIR/versions/node/v10.6.3

# nvm alias default
$ nvm alias default

# symlink zsh & zim config files into home
$ find dotfiles/zsh-zim -name .z\* -exec ln -vs "{}" ~ ';'
```

## Depricated Setup

Created repo in home. Require files to be explicitly added.

```sh
$ cd
$ git init
$ echo "*" >> ~/.git/info/exclude
$ git add -f ~/.vimrc
```

#### applications
 - screen
 - tmux
 - vim
 - zsh
