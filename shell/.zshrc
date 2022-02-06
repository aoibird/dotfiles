#!/usr/bin/env zsh
#
# ~/.zshrc
#


if [ -f ~/.shell_envvars ]; then source ~/.shell_envvars; fi
if [ -f ~/.shell_aliases ]; then source ~/.shell_aliases; fi
if [ -f ~/.shell_secret ]; then source ~/.shell_secret; fi

# Functions
function crun { make $1 && ./$1; }


# Completion
autoload -Uz compinit -d $XDG_CACHE_HOME/zcompdump && compinit -d $XDG_CACHE_HOME/zcompdump
zstyle ':completion:*' menu select
zstyle :compinstall filename "$HOME/.zshrc"


# Prompt style: last/two %
export PROMPT='%F{green}%B%2~%b%f %# '
export RPROMPT='%F{blue}%D{%H:%M}%f'


# History
export HISTFILE=$XDG_DATA_HOME/zsh/history
export HISTSIZE=1000000
export SAVEHIST=1000000
export HISTTIMEFORMAT="[%F %T] "

setopt INC_APPEND_HISTORY_TIME
setopt EXTENDED_HISTORY
setopt SHARE_HISTORY


# Refresh the timestamp every minute
TMOUT=60
TRAPALRM() {
    zle;
    zle reset-prompt
}

# Misc
