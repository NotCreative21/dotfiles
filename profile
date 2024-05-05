# /etc/profile

# System wide environment and startup programs.

appendpath () {
    case ":$PATH:" in
        *:"$1":*)
            ;;
        *)
            PATH="${PATH:+$PATH:}$1"
    esac
}

# Set our default path (/usr/sbin:/sbin:/bin included for non-Void chroots)
appendpath '/usr/local/sbin'
appendpath '/usr/local/bin'
appendpath '/usr/bin'
appendpath '/usr/sbin'
appendpath '/sbin'
appendpath '/bin'
unset appendpath

export HOME="/home/sean"
export PATH=$PATH:$HOME/.config/scripts
export PATH=$PATH:/usr/local/bin
export XDG_CONFIG_HOME="$HOME/.config"
export CARGO_HOME="$HOME/.config/.cargo"
export RUSTUP_HOME="$HOME/.config/.rustup"
export LESSHISTFILE=-
export PULSE_COOKIE="$HOME/.config/"
export COLORTERM="truecolor"
export _JAVA_AWT_WM_NONREPARENTING=1
export TERM="xterm-256color"
export XAUTHORITY="$HOME/.config/.Xauthority"
export ZDOTDIR="$HOME/.config/"
export XDG_RUNTIME_DIR=/run/user/$UID

#export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/lib32
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/lib

export PATH

# Set default umask
umask 022

# Load profiles from /etc/profile.d
if [ -d /etc/profile.d/ ]; then
	for f in /etc/profile.d/*.sh; do
		[ -r "$f" ] && . "$f"
	done
	unset f
fi
