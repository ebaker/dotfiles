# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Path additions
export PATH=/usr/local/share/npm/bin:/usr/local/bin:/opt/local/bin:/opt/local/sbin:$HOME/bin:$HOME/Library/Haskell/bin:$PATH
export PATH=/Library/TeX/Root/bin/x86_64-darwin:$PATH

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
#ZSH_THEME="robbyrussell"
#ZSH_THEME="zanshin"
ZSH_THEME="aussiegeek"

# Example aliases
alias zshconfig="mate ~/.zshrc"
alias ohmyzsh="mate ~/.oh-my-zsh"
alias la="ls -al"

# aliases
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias df='df -h'
alias la='ls -A'
alias ll='ls -l'
alias gmail='/Applications/Firefox.app/Contents/MacOS/firefox http://www.gmail.com'
alias mail='/Applications/Firefox.app/Contents/MacOS/firefox http://mail.lessthan3.com'

# Git related
alias gs='git status'
alias gc='git commit -m'
alias ga='git add'
alias gd='git diff'
alias gb='git branch'
alias gl='git log'
alias gsb='git show-branch'
alias gco='git checkout'
alias gg='git grep'
alias gk='gitk --all'
alias gr='git rebase'
alias gri='git rebase --interactive'
alias gcp='git cherry-pick'
alias grm='git rm'

unalias run-help
autoload run-help
HELPDIR=/usr/local/share/zsh/help

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git rails ruby zshmarks)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting

function collapse_pwd {
    echo $(pwd | sed -e "s,^$HOME,~,")
}

function prompt_char {
    git branch >/dev/null 2>/dev/null && echo '±' && return
    hg root >/dev/null 2>/dev/null && echo '☿' && return
    echo '○'
}

function battery_charge {
    echo `$BAT_CHARGE` 2>/dev/null
}

function virtualenv_info {
    [ $VIRTUAL_ENV ] && echo '('`basename $VIRTUAL_ENV`') '
}

function hg_prompt_info {
    hg prompt --angle-brackets "\
< on %{$fg[magenta]%}<branch>%{$reset_color%}>\
< at %{$fg[yellow]%}<tags|%{$reset_color%}, %{$fg[yellow]%}>%{$reset_color%}>\
%{$fg[green]%}<status|modified|unknown><update>%{$reset_color%}<
patches: <patches|join( → )|pre_applied(%{$fg[yellow]%})|post_applied(%{$reset_color%})|pre_unapplied(%{$fg_bold[black]%})|post_unapplied(%{$reset_color%})>>" 2>/dev/null
}

PROMPT='
%{$fg[magenta]%}%n%{$reset_color%} at %{$fg[yellow]%}%m%{$reset_color%} in %{$fg_bold[green]%}$(collapse_pwd)%{$reset_color%}$(hg_prompt_info)$(git_prompt_info)
$(virtualenv_info)$(prompt_char) '

RPROMPT='$(battery_charge)'

ZSH_THEME_GIT_PROMPT_PREFIX=" on %{$fg[magenta]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[green]%}!"
ZSH_THEME_GIT_PROMPT_UNTRACKED="%{$fg[green]%}?"
ZSH_THEME_GIT_PROMPT_CLEAN=""

setopt no_share_history

# peak into global history
function peek-history () {
        zle set-local-history
        zle up-history
        zle set-local-history
}
zle -N peek-history
bindkey "^[Oa" peak-history

# git autocomplete speed up
__git_files () { 
    _wanted files expl 'local files' _files     
}

# cd bookmarks 
# http://blog.angeloff.name/post/2010/08/29/cd-with-bookmarks-and-auto-completion-for-zsh/
ZSH_BOOKMARKS="$HOME/.zshbookmarks"
 
function cdb_edit() {
  $EDITOR "$ZSH_BOOKMARKS"
}
 
function cdb() {
  local index
  local entry
  index=0
  for entry in $(echo "$1" | tr '/' '\n'); do
    if [[ $index == "0" ]]; then
      local CD
      CD=$(egrep "^$entry\\s" "$ZSH_BOOKMARKS" | sed "s#^$entry\\s\+##")
      if [ -z "$CD" ]; then
        echo "$0: no such bookmark: $entry"
        break
      else
        cd "$CD"
      fi
    else
      cd "$entry"
      if [ "$?" -ne "0" ]; then
        break
      fi
    fi
    let "index++"
  done
}
 
function _cdb() {
  reply=(`cat "$ZSH_BOOKMARKS" | sed -e 's#^\(.*\)\s.*$#\1#g'`)
}
 
compctl -K _cdb cdb
