# dotfiles

## Git Submodules Workflow

```sh
# clone dotfiles repo
$ git clone --recursive git@github.com:ebaker/dotfiles.git

# git submopdules update if there are nested submodules:
git submodule update --recursive

# pull all changes in the repo including changes in the submodules
git pull --recurse-submodules

# pull all changes for the submodules
git submodule update --remote
```

## Installation Script

```sh
$ source ./install.sh
```

