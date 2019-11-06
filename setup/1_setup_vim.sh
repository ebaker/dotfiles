#!/bin/bash

DOTFILES_DIR=$(pwd)

# backup vim files
find $HOME -maxdepth 1 -name .vimrc -exec mv {} {}.bak \;
find $HOME -maxdepth 1 -name .vim -exec mv {} {}.bak \;

# setup vim symlinks
ln -vs $DOTFILES_DIR/.vim $HOME/.vim
ln -vs $DOTFILES_DIR/.vimrc $HOME/.vimrc
