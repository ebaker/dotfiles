#!/bin/bash

DOTFILES_DIR=$(pwd)

# backup ~/.z* files
find $HOME -maxdepth 1 -name .zsh -exec mv {} {}.bak \;
find $HOME -maxdepth 1 -name .zshrc -exec mv {} {}.bak \;
find $HOME -maxdepth 1 -name .zshrc.local -exec mv {} {}.bak \;
find $HOME -maxdepth 1 -name .zlogin -exec mv {} {}.bak \;
find $HOME -maxdepth 1 -name .zimrc -exec mv {} {}.bak \;
find $HOME -maxdepth 1 -name .zim -exec mv {} {}.bak \;

# setup zsh & zim symlinks
find $DOTFILES_DIR/zsh-zim -name .z\* -exec ln -vs "{}" $HOME ';'
