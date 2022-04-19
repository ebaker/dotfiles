#!/bin/bash

# Setup script for setting up a new macos machine
echo "Starting setup"
# install xcode CLI
xcode-select --install

# Check for Homebrew to be present, install if it's missing
if test ! $(which brew); then
    echo "Installing homebrew..."
    ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

# Update homebrew recipes
brew update

brew install $(<brew-packages.txt)
brew install --cask $(<brew-cask-packages.txt)

# Cleanup brew
echo "Cleaning up..."
brew cleanup

echo "Configuring OS..."
# Set fast key repeat rate
defaults write NSGlobalDomain KeyRepeat -int 0
# Require password as soon as screensaver or sleep mode starts
# defaults write com.apple.screensaver askForPassword -int 1
# defaults write com.apple.screensaver askForPasswordDelay -int 0
# Show filename extensions by default
defaults write NSGlobalDomain AppleShowAllExtensions -bool true
