# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="noon"

# Set to this to use case-sensitive completion
CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git)

source $ZSH/oh-my-zsh.sh
unsetopt correct_all

# Customize to your needs...
export PATH=/home/noon/dev/silky-github/utils/find-todo:/usr/local/texlive/2011/bin/x86_64-linux:/usr/lib/lightdm/lightdm:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/home/noon/bin:/home/noon/.cabal/bin

export GUROBI_HOME="/opt/gurobi400/linux64"
export PATH="${PATH}:${GUROBI_HOME}/bin"
export LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:${GUROBI_HOME}/lib:/usr/local/lib"

export BOOST_BUILT_LIBS="/usr/include/boost_1_47_0/stage/lib"
export LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:${BOOST_BUILT_LIBS}"

export CPLUS_INCLUDE_PATH="${CPLUS_INCLUDE_PATH}:/usr/include/boost_1_47_0"

alias gg="sudo apt-get install"
alias df="df -h"
# heman foo runs man foo || foo -- help
alias man='heman.sh'
alias sel='noglob sel'
alias l='ls -lFh'
alias ll='ls -lFhA'
