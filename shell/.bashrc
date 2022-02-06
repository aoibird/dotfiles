#!/usr/bin/env bash
#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

if [ -f ~/.shell_envvars ]; then source ~/.shell_envvars; fi
if [ -f ~/.shell_aliases ]; then source ~/.shell_aliases; fi
if [ -f ~/.shell_secret ]; then source ~/.shell_secret; fi

# username: \u, hostname: \h, working directory: \W
# [username@hostname basename] $
# export PS1="[\[$(tput bold)\]\[$(tput sgr0)\]\[\033[38;5;9m\]\u@\h\[$(tput sgr0)\]\[$(tput sgr0)\] \[$(tput bold)\]\W\[$(tput sgr0)\]] \\$\[$(tput sgr0)\] "

# basename $
export PS1="\[$(tput bold)\]\[\033[38;5;208m\]\W \\$ \[$(tput sgr0)\]"

HISTFILE=$XDG_DATA_HOME/bash/history
HISTFILESIZE=100000
