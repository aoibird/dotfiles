#!/usr/bin/env zsh
#
# ~/.zshrc
#

if [ -f ~/.aliases ]; then
    . ~/.aliases
fi

if [ -f ~/.aliases_secret ]; then
    . ~/.aliases_secret
fi

if [ -f ~/.env ]; then
    . ~/.env
fi

# Functions
function crun { make $1 && ./$1; }


# Completion
autoload -Uz compinit -d ~/.cache/zcompdump && compinit -d ~/.cache/zcompdump
zstyle ':completion:*' menu select
zstyle :compinstall filename "$HOME/.zshrc"


# Prompt style: last/two %
export PROMPT='%F{green}%B%2~%b%f %# '
export RPROMPT='%F{blue}%D{%H:%M}%f'


# History
export HISTFILE=~/.cache/zsh_history
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
