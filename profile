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

export TZ="America/New_York"
export CUDA_CACHE_PATH="$XDG_CACHE_HOME"/nv
export SHOME="/home/sean"
export PATH=$PATH:$SHOME/.config/scripts
export PATH=$PATH:/usr/local/bin
export PATH=$PATH:/home/sean/.config/cabal/bin
export PATH=$PATH:/usr/local/cuda/bin
export XDG_CONFIG_HOME="$SHOME/.config"
export CARGO_HOME="$SHOME/.config/.cargo"
export RUSTUP_HOME="$SHOME/.config/.rustup"
export CABAL_DIR="$SHOME/.config/cabal"
export LESSHISTFILE=-
export PULSE_COOKIE="$SHOME/.config/"
export COLORTERM="truecolor"
export _JAVA_AWT_WM_NONREPARENTING=1
export PYENV_ROOT="$SHOME/.config"
export TERM="xterm-256color"
export XAUTHORITY="$SHOME/.config/.Xauthority"
export ZDOTDIR="$SHOME/.config/"
export STACK_ROOT="$SHOME/.config/stack"
export XDG_RUNTIME_DIR=/run/user/$UID
export XDG_DATA_HOME="$SHOME/.local/share"
export XDG_STATE_HOME="$SHOME/.local/state"
export XDG_CACHE_HOME="$SHOME/.cache"
export STACK_ROOT="$XDG_DATA_HOME"/stack
export STACK_XDG=1
export GHCUP_USE_XDG_DIRS=true
export yarn="--use-yarnrc $XDG_CONFIG_HOME/yarn/config"
export PYTHONSTARTUP="$XDG_CONFIG_HOME"/python/pythonrc
export _JAVA_OPTIONS=-Djava.util.prefs.userRoot="$XDG_CONFIG_HOME"/java

#export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/lib32
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/lib
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/cuda/lib64
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/opt/libtorch/lib

export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/opt/tensorrt/lib
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/opt/cudnn/lib

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
