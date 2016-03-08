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
DISABLE_AUTO_UPDATE="true"

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
export PATH=$HOME/.cabal/bin:/home/noon/bin:/home/noon/dev/silky-github/utils/find-todo:/usr/local/texlive/2011/bin/x86_64-linux:/usr/lib/lightdm/lightdm:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/home/noon/.cabal/bin:/home/noon/dev/ext/quipper/quipper-0.4/quipper/scripts:/usr/lib/mono/4.0/:/home/noon/node_modules/.bin:/home/noon/.rvm/bin:/home/noon/tools/MiniZinc:/home/noon/tools/swift-2.2/usr/bin/:$PATH

export GUROBI_HOME="/opt/gurobi400/linux64"
export PATH="${PATH}:${GUROBI_HOME}/bin"
export LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:${GUROBI_HOME}/lib:/usr/local/lib"
export EXT_LLVM_DIR=/home/noon/tools/llvm/build
export BOOST_BUILT_LIBS="/usr/include/boost_1_47_0/stage/lib"
export LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:${BOOST_BUILT_LIBS}"

export CPLUS_INCLUDE_PATH="${CPLUS_INCLUDE_PATH}:/usr/include/boost_1_47_0"

# 'repl' loads up ghci (which is actually 'ghci-color')
alias repl='cabal repl --with-ghc=ghci'
alias gg="sudo apt-get install"
alias df="df -h"
alias agp="ag -G '.py$'"
alias agh="ag -G '.hs$'"
# heman foo runs man foo || foo -- help
alias man='heman.sh'
alias sel='noglob sel'
alias l='ls -lFh'
alias ll='ls -lFhA'
alias pg='ps aux | grep'
# Stuff for taskwarrior
alias task='~/dev/heroku/cupduck/update_task_status.py && task'
alias t='task $TW_FILTER'
alias ta='t add $TW_FILTER'
alias c='clear && task'
alias cal='task cal'
alias ca='c && cal'
alias shake='runhaskell Shakefile'
alias today='conscript sh -c "clear && task due:today"'
alias gitjk="history 10 | tac | gitjk_cmd"

export WORKON_HOME=$HOME/.virtualenvs
export PROJECT_HOME=$HOME/dev
source /usr/local/bin/virtualenvwrapper.sh

source /usr/local/share/chruby/chruby.sh


# added by travis gem
[ -f /home/noon/.travis/travis.sh ] && source /home/noon/.travis/travis.sh


export PATH=./.cabal-sandbox/bin:/home/noon/torch/install/bin:/usr/racket/bin:/usr/local/cuda-7.0/bin:/home/noon/.local/bin:/opt/ghc/7.10.3/bin:$PATH
export LD_LIBRARY_PATH=/home/noon/torch/install/lib:/usr/local/cuda-7.0/lib64:$LD_LIBRARY_PATH
export DYLD_LIBRARY_PATH=/home/noon/torch/install/lib:$DYLD_LIBRARY_PATH

export CUDA_HOME=/usr/local/cuda

export NVM_DIR="/home/noon/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm
