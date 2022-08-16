# Case-sensitive autocompleting
export CASE_SENSITIVE=true
CASE_SENSITIVE=true

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
  # From: https://github.com/chisui/zsh-nix-shell
  # zgen load /home/noon/.zgen/robbyrussell/oh-my-zsh-master/custom/plugins/nix-shell

  # generate the init script from plugins above
  zgen save
fi


# Start from scratch with path, so we know what's happening.
export PATH="/usr/local/sbin"
export PATH="/usr/local/sbin:$PATH"
export PATH="/usr/local/bin:$PATH"
export PATH="/usr/sbin:$PATH"
export PATH="/usr/bin:$PATH"
export PATH="/sbin:$PATH"
export PATH="/bin:$PATH"
export PATH="/snap/bin:$PATH"

# Conda
export PATH="$HOME/tools/miniconda3/bin/:$PATH"

# Emacs
export PATH="$HOME/.emacs.d/bin:$PATH"

# Haskell
export PATH="$HOME/.local/bin:$PATH"

# Rust
export PATH="$HOME/.cargo/bin:$PATH"


export PATH="$HOME/.local/bin:$PATH"


alias v="nvim"

# Open in read-only mode. Useful for searching.
alias vr="nvim -R"
alias vv="nvim -R"

alias e="emacs"
alias vim="nvim"
alias gg="sudo apt install"
alias df="df -h -t ext4"
alias g="stack ghci"
alias p="python"
alias du="du -h"
# Don't return matches on stupidly long lines
alias rg="rg -M 1000"
alias rr="commando -c echo | grep --line-buffered Modified | conscript"
alias jn="jupyter notebook"
alias r='ranger'

alias b="stack build"
alias bf="stack build --fast"

# Do all my upgrades ...
alias upg="sudo apt update && sudo apt upgrade -y && stack upgrade --git --git-branch stable"

# Git-Related
alias gpr='git pull --rebase'
alias gf='git fetch'
alias st='git status'
alias ci='git commit -m'
alias co='git checkout'
alias pp='git push'
alias gc='git clone --recursive'

alias sa='source activate `basename \`pwd\``'

alias shh='ssh -q'
alias cl='clash.clashi'

alias yank='yank-cli'

alias tv='tidy-viewer'

# Shorter docker-compose
alias dc='docker-compose'
alias dcl='docker-compose logs -f'

alias bb='stack build --fast --file-watch'

# Nix
alias n='nix-shell'

# Task-Warrior
alias t='task'

# Env vars
export TERM=xterm-256color
export EDITOR=/home/noon/.local/bin/nvim
export LC_ALL=en_AU.UTF-8
export LANG=en_AU.UTF-8
export LANGUAGE=en_AU.UTF-8


RPG_CLI=/home/noon/dev/ext/rpg-cli/target/release/rpg-cli
rpg () {
   $RPG_CLI "$@"
   cd "$($RPG_CLI --pwd)"
}


# Case-sensitive autocompleting
export CASE_SENSITIVE=true
CASE_SENSITIVE=true

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
if [ -e /home/noon/.nix-profile/etc/profile.d/nix.sh ]; then . /home/noon/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

eval "$(direnv hook zsh)"

[ -f "/home/noon/.ghcup/env" ] && source "/home/noon/.ghcup/env" # ghcup-en
