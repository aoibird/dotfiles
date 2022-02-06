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


# Aliases
alias gitgraph='git log --graph --oneline --branches '
alias gcc='gcc -Wall -std=c11 -g '
alias g++='g++ -Wall -std=c++11 -g '
alias py='python '
alias py2='python2 '
alias py3='python3 '
if [ "$(uname)" = "Darwin" ]; then
    alias firefox='/Applications/Firefox.app/Contents/MacOS/firefox-bin '
    alias ls='ls -Gh '
    alias less='less -G '
    alias echopath="echo $PATH | tr ':' '\n'"
fi
if [ "$(uname)" = "Linux" ]; then
    # https://wiki.archlinux.org/index.php/Color_output_in_console
    alias ls='ls -v --color=auto --time-style=long-iso '
    alias diff='diff --color=auto '
    alias grep='grep --color=auto '
    alias ip='ip -color=auto '
fi
alias dateiso='date +"%Y-%m-%d"'
alias datetimeiso='date +"%Y-%m-%d %H:%M"'
alias gg='wget www.google.com -O /tmp/gg.txt'
alias gd='wget www.baidu.com -O /tmp/gd.txt'
alias ga='git-annex'
alias wl='iwctl station wlan0 '
alias wlup='iwctl station wlan0 connect '
alias wldown='iwctl station wlan0 disconnect'
alias lo2pdf='libreoffice --headless --convert-to pdf '
alias sy='sudo systemctl '
alias syu='systemctl --user '


# Environment variables
if [ "$(uname)" = "Darwin" ]; then
    # Homebrew
    export PATH=/usr/local/sbin:"$PATH"
fi

export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_DIRS="$HOME/.local/share:$XDG_DATA_DIRS"

export CARGO_HOME=$XDG_DATA_HOME/cargo
export LESSHISTFILE=$XDG_DATA_HOME/less/history
export IPYTHONDIR=$XDG_DATA_HOME/ipython
export KDEHOME=$XDG_DATA_HOME/kde
export RUSTUP_HOME=$XDG_DATA_HOME/rustup
export GOPATH=$XDG_DATA_HOME/go
export LYX_USERDIR_23x=$XDG_DATA_HOME/lyx
export TEXMFVAR=$XDG_DATA_HOME/texlive/texmf-var
export TEXMFHOME=$XDG_DATA_HOME/texmf
export CONDA_ENVS_PATH=$XDG_DATA_HOME/conda
export KERAS_HOME=$XDG_DATA_HOME/keras
export JAVA_OPTS="-Djava.util.prefs.userRoot=$XDG_DATA_HOME/java"
export TEXMACS_HOME_PATH=$XDG_DATA_HOME/texmacs

export GTK2_RC_FILES=$XDG_CONFIG_HOME/gtk-2.0/gtkrc
export GTK_RC_FILES=$XDG_CONFIG_HOME/gtk/gtkrc
export JUPYTER_CONFIG_DIR=$XDG_CONFIG_HOME/jupyter
export WGETRC=$XDG_CONFIG_HOME/wget/wgetrc
export TEXMFCONFIG=$XDG_CONFIG_HOME/texlive/texmf-config

export XAUTHORITY=$XDG_CACHE_HOME/Xauthority

export PATH=$HOME/.local/bin:$PATH
export PATH=$HOME/dotfiles/scripts:$PATH

# Misc
