" Noons .vimrc.
"
" Based on Steve Losh's .vimrc: http://bitbucket.org/sjl/dotfiles/src/tip/vim
"
" Author: Noon Silk <noonsilk@gmail.com>
" Location: https://github.com/silky/dotfiles/blob/master/.vimrc


" New ----- {{{

" Use stylish haskell instead of par for haskell buffers
autocmd FileType haskell let &formatprg="stylish-haskell"
