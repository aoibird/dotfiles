#!/usr/bin/env zsh
#
# ~/.zshrc
#

function safesource { [[ -s $1 ]] && source $1 }

safesource ~/.shell_envvars
safesource ~/.shell_aliases
safesource ~/.shell_secret

# Functions
function crun { make $1 && ./$1; }


# Completion
autoload -Uz compinit -d $XDG_CACHE_HOME/zcompdump && compinit -d $XDG_CACHE_HOME/zcompdump
zstyle ':completion:*' menu select
zstyle :compinstall filename "$HOME/.zshrc"

# Prompt style: last/two %
export PROMPT='%F{green}%B%2~%b%f %# '
export RPROMPT='%F{blue}%D{%H:%M}%f'

# Auto-suggestions
# https://github.com/zsh-users/zsh-autosuggestions
# safesource $XDG_CONFIG_HOME/zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
safesource /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
safesource /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

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
