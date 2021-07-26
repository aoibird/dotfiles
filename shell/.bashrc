#!/usr/bin/env bash
#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

if [ -f ~/.aliases ]; then
    . ~/.aliases
fi

if [ -f ~/.env ]; then
    . ~/.env
fi

# username: \u, hostname: \h, working directory: \W
# [username@hostname basename] $
# export PS1="[\[$(tput bold)\]\[$(tput sgr0)\]\[\033[38;5;9m\]\u@\h\[$(tput sgr0)\]\[$(tput sgr0)\] \[$(tput bold)\]\W\[$(tput sgr0)\]] \\$\[$(tput sgr0)\] "

# basename $
export PS1="\[$(tput bold)\]\[\033[38;5;208m\]\W \\$ \[$(tput sgr0)\]"

HISTFILE=$HOME/.cache/bash_history
HISTFILESIZE=100000
