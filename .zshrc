#
# User configuration sourced by interactive shells
#

# Define zim location
export ZIM_HOME=${ZDOTDIR:-${HOME}}/.zim

# Start zim
[[ -s ${ZIM_HOME}/init.zsh ]] && source ${ZIM_HOME}/init.zsh

# Zim Git alias prefix set to 'G'
# https://github.com/zimfw/zimfw/blob/master/modules/git/README.md
zstyle ':zim:git' aliases-prefix 'G'

# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '~/.zshrc'

# better history searching w/ arrow keys
# https://coderwall.com/p/jpj_6q/zsh-better-history-searching-with-arrow-keys
autoload -U up-line-or-beginning-search
autoload -U down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
bindkey "^[[A" up-line-or-beginning-search # Up
bindkey "^[[B" down-line-or-beginning-search # Down

# autoload -Uz compinit
compinit
# End of lines added by compinstall

# zsh speed up - https://medium.com/@dannysmith/little-thing-2-speeding-up-zsh-f1860390f92
autoload -Uz compinit
for dump in ~/.zcompdump(N.mh+24); do
  compinit
done
compinit -C

# zsh-autosuggestions
source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh

# docker - aliases
# source ~/.zsh/docker-alias/zshrc

# eliot - git
# unalias gs
alias gs='git status'
alias gc='git commit -m'
alias gcam='git commit -am'
alias ga='git add'
alias gd='git diff'
alias gb='git branch'
alias gl='git log'
alias gsb='git show-branch'
alias go='git checkout'
alias gg='git grep'
alias gk='gitk --all'
alias gr='git rebase'
alias gri='git rebase --interactive'
alias gcp='git cherry-pick'
alias grm='git rm'

## eliot - docker
alias d=docker
alias dps='docker ps'
alias dl='docker logs'
alias di='docker images'
alias dcst='docker container stop $(docker container ls -aq)' # docker stop (all containers)
alias dcrm='docker container rm $(docker container ls -aq)'

# exa
alias l='exa'
alias la='exa -a'
alias ll='exa -lah'
alias ls='exa --color=auto'

# override system binaries
alias cat='bat'
alias find='fd'
alias grep='rg'


# eliot functions

# mkdir & cd - https://unix.stackexchange.com/questions/125385/combined-mkdir-and-cd
cdd ()
{
    mkdir -p -- "$1" &&
      cd -P -- "$1"
}


# docker autocomplete - https://medium.com/@MicoDer/docker-zsh-autocomplete-and-denter-on-macos-easy-tutorial-630c46836652

ZSHRC_LOCAL=.zshrc.local
if [ -f ${ZSHRC_LOCAL} ]; then
   source ${ZSHRC_LOCAL}
fi

# nvm
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# place this after nvm initialization!
autoload -U add-zsh-hook
load-nvmrc() {
  local node_version="$(nvm version)"
  local nvmrc_path="$(nvm_find_nvmrc)"

  if [ -n "$nvmrc_path" ]; then
    local nvmrc_node_version=$(nvm version "$(cat "${nvmrc_path}")")

    if [ "$nvmrc_node_version" = "N/A" ]; then
      nvm install
    elif [ "$nvmrc_node_version" != "$node_version" ]; then
      nvm use
    fi
  elif [ "$node_version" != "$(nvm version default)" ]; then
    echo "Reverting to nvm default version"
    nvm use default
  fi
}
add-zsh-hook chpwd load-nvmrc
load-nvmrc

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
