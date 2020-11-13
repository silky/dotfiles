# zstyle ':completion:*' list-colors "${(@s.:.)LS_COLORS}"
autoload -Uz compinit; compinit
autoload -U colors && colors

# load zgen
source "$HOME/.zgen/zgen.zsh"


# if the init scipt doesn't exist
if ! zgen saved; then
  # specify plugins here
  zgen oh-my-zsh
  zgen oh-my-zsh plugins/sudo
  zgen oh-my-zsh plugins/command-not-found
  # zgen load silky/noon.zsh-theme noon.zsh-theme
  zgen load /home/noon/dev/noon.zsh-theme/noon-light.zsh-theme
  
  # generate the init script from plugins above
  zgen save
fi

# Linux-Brew
export PATH="/home/linuxbrew/.linuxbrew/bin:$PATH"

# Haskell
export PATH="$HOME/.local/bin:$PATH"

# Rust
export PATH="$HOME/.cargo/bin:$PATH"



alias v=nvim
alias gg="sudo apt install"
alias df="df -h -t ext4"
alias g="stack ghci"
alias p="python"
alias b="stack build"
alias du="du -h"
# Don't return matches on stupidly long lines
alias rg="rg -M 1000"
alias sr="stack run --"
alias rr="commando -c echo | grep --line-buffered Modified | conscript"
alias 📚="stack"

# Do all my upgrades ...
alias upg="sudo apt update && sudo apt upgrade -y && stack upgrade --git --git-branch stable"

# Git-Related
alias gpr='git pull --rebase'
alias st='git status'
alias ci='git commit -m'
alias co='git checkout'
alias pp='git push'
alias gc='git clone --recursive'
alias sa='source activate `basename \`pwd\``'
alias shh='ssh -q'
alias cl='clash.clashi'

# Shorter docker-compose
alias dc='docker-compose'
alias dcl='docker-compose logs -f'



# Up (with detach) and follow
upf() {
  if [ "$1" != "" ]
  then
    docker-compose up -d "$1" && docker-compose logs -f "$1"
  else
    echo "upf: Need an argument; the image to run!"
  fi
}

# Env vars
export TERM=xterm-256color
export EDITOR=nvim

# Case-sensitive autocompleting
export CASE_SENSITIVE=true
CASE_SENSITIVE=true
#
# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/noon/tools/miniconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/noon/tools/miniconda3/etc/profile.d/conda.sh" ]; then
        . "/home/noon/tools/miniconda3/etc/profile.d/conda.sh"
    else
        export PATH="/home/noon/tools/miniconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
[ -f "/home/noon/.ghcup/env" ] && source "/home/noon/.ghcup/env" # ghcup-env
