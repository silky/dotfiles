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
  zgen oh-my-zsh themes/ys
  
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


export TERM=xterm-256color

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
