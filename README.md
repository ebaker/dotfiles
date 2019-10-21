# dotfiles

```sh
# clone dotfiles repo
$ git clone git@github.com:ebaker/dotfiles.git


# symlink zsh & zim config files into home
$ find dotfiles/zsh-zim -name \* -exec ln -vs "{}" ~ ';'
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
