#!/usr/bin/env bash
#
# ~/.bashrc
#

if [ -f ~/.aliases ]; then
    . ~/.aliases
fi

if [ -f ~/.env ]; then
    . ~/.env
fi

# [username@hostname basename] $
# export PS1="[\[$(tput bold)\]\[$(tput sgr0)\]\[\033[38;5;9m\]\u@\h\[$(tput sgr0)\]\[$(tput sgr0)\] \[$(tput bold)\]\W\[$(tput sgr0)\]] \\$\[$(tput sgr0)\] "

# basename $
export PS1="\[$(tput bold)\]\[\033[38;5;208m\]\W \\$ \[$(tput sgr0)\]"

HISTFILESIZE=1000