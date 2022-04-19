#!/bin/bash

DOTFILES_DIR=$(pwd)

# emacs-plus
brew tap railwaycat/emacsmacport
brew install --cask emacs-mac --with-spacemacs-icon --with-native-comp
brew link emacs-mac
ln -s /usr/local/opt/emacs-mac/Emacs.app /Applications

# backup ~/.emacs.d files
find $HOME -maxdepth 1 -name .emacs.d -exec mv {} {}.bak \;

# setup .emacs.d symlink
ln -vs $DOTFILES_DIR/.emacs.d $HOME/.emacs.d
