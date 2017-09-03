# load zgen
source "${HOME}/.zgen/zgen.zsh"

# Case-sensitive autocompleting
CASE_SENSITIVE="true"

# if the init scipt doesn't exist
if ! zgen saved; then

  # specify plugins here
  zgen oh-my-zsh
  zgen oh-my-zsh plugins/sudo
  zgen oh-my-zsh plugins/command-not-found
  zgen load silky/noon.zsh-theme noon.zsh-theme
  
  # generate the init script from plugins above
  zgen save
fi

# Conda
export PATH="/home/noon/tools/miniconda3/bin:$PATH"

# Haskell
export PATH="/home/noon/.local/bin:$PATH"

# Rust
export PATH="/home/noon/.cargo/bin:$PATH"

# Alias'
alias v=nvim
alias gg="sudo apt-get install"

## Git-Related
alias gpr='git pull --rebase'
alias st='git status'
alias ci='git commit -m'
alias pp='git push'
alias gc='git clone --recursive'

# Env vars
export TERM=xterm-256color
export EDITOR=nvim


# Auto-added things follow ...
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
