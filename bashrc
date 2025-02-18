# /etc/skel/.bashrc
#
# This file is sourced by all *interactive* bash shells on startup,
# including some apparently interactive shells such as scp and rcp
# that can't tolerate any output.  So make sure this doesn't display
# anything or bad things will happen !


# Test for an interactive shell.  There is no need to set anything
# past this point for scp and rcp, and it's important to refrain from
# outputting anything in those cases.
if [[ $- != *i* ]] ; then
	# Shell is non-interactive.  Be done now!
	return
fi

xset r rate 300 30

alias vvim="vim"
alias sudo='doas'
alias rm='rm -v -I'
alias ll='exa --long --header'
alias grep='rg'
alias find='fd'
alias ls='ls --color=auto'
alias sd="nsxiv"
alias v="nvim"
alias vim="nvim"
alias wget="wget --hsts-file ~/.config/wget/wget-hsts"
export TERM='xterm-256color'
export TERMINAL="kitty"
export terminal="kitty"
export COLORTERM="truecolor"

PS1="[\\[\033[90m\]\w\\[\033[37m\]\033[0m\]] \033[39m\]"
. "$HOME/.config/.cargo/env"
todo
