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
ln -vs $DOTFILES_DIR/.zsh $HOME/.zsh
ln -vs $DOTFILES_DIR/.zshrc $HOME/.zshrc
ln -vs $DOTFILES_DIR/.zshrc.local $HOME/.zshrc.local
ln -vs $DOTFILES_DIR/.zlogin $HOME/.zlogin
ln -vs $DOTFILES_DIR/.zimrc $HOME/.zimrc
ln -vs $DOTFILES_DIR/.zim $HOME/.zim
