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

autoload -Uz compinit && compinit
zstyle ':completion:*' menu select
zstyle :compinstall filename "$HOME/.zshrc"

# last/two %
export PROMPT='%F{green}%B%2~%b%f %# '
export RPROMPT='%F{blue}%D{%H:%M}%f'

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
if [ -e /home/aoi/.nix-profile/etc/profile.d/nix.sh ]; then . /home/aoi/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
