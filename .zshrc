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

# Conda
export PATH="$HOME/tools/miniconda3/bin:$PATH"

# Haskell
export PATH="$HOME/.local/bin:$PATH"

# Rust
export PATH="$HOME/.cargo/bin:$PATH"

# Unity
export PATH="$HOME/tools/unity/Editor:$PATH"

# Node
export PATH="$HOME/tools/node/bin:$PATH"
export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"

# Go
# export GOPATH="$HOME/tools/gocode"
# export GOROOT="$HOME/tools/go"
# export PATH="$GOPATH/bin:$PATH"
# export PATH="$GOROOT/bin:$PATH"


# Ruby
export PATH="$HOME/.rbenv/bin:$PATH"
export PATH="$HOME/.rbenv/plugins/ruby-build/bin:$PATH"
export PATH="/home/noon/.rbenv/shims:${PATH}"
export RBENV_SHELL=zsh
source '/home/noon/.rbenv/libexec/../completions/rbenv.zsh'
command rbenv rehash 2>/dev/null
rbenv() {
  local command
  command="$1"
  if [ "$#" -gt 0 ]; then
    shift
  fi

  case "$command" in
  rehash|shell)
    eval "$(rbenv "sh-$command" "$@")";;
  *)
    command rbenv "$command" "$@";;
  esac
}

# Alias'
pub() { mosquitto_pub -h $1 -t $2 -m “$3”; }


alias v=nvim
alias gg="sudo apt install"
alias df="df -h"
alias g="stack ghci"
alias p="python"
alias b="stack build"
alias du="du -h"
# Don't return matches on stupidly long lines
alias rg="rg -M 1000"
alias sr="stack run --"
alias rr="commando -c echo | grep --line-buffered Modified | conscript"
alias n='konsole --workdir `pwd` &'
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

# Env vars
export TERM=xterm-256color
export EDITOR=nvim

# Tools
# export PATH="$HOME/tools/storyboarder:$PATH"

#
export AIKO_MQTT_HOST=localhost
# Case-sensitive autocompleting
export CASE_SENSITIVE=true
CASE_SENSITIVE=true

# Auto-added things follow ...
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
