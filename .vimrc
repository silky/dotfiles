" Noons .vimrc.
"
" Based on Steve Losh's .vimrc: http://bitbucket.org/sjl/dotfiles/src/tip/vim
"
" Author: Noon Silk <noonsilk@gmail.com>
" Last Modified: 19-Oct-2011

" Initialisation -------------------------------------------------------------- {{{

filetype off
silent! call pathogen#infect()
silent! call pathogen#helptags()
filetype plugin indent on

set nocompatible

" }}}
" Colors and Fonts ------------------------------------------------------------ {{{

syntax on
set background=dark
colorscheme noon

" set guifont=Bitstream\ Vera\ Sans\ Mono\ 10
" If you have it, this is somewhat preferable, otherwise use the above.
set guifont=Akkurat-Mono\ 8

" }}}
" General Options ------------------------------------------------------------- {{{

set winheight=30 " Autosize window to this height.

set encoding=utf-8
set modelines=0

set cpoptions+=I "do NOT revert tabbing I have specifically set.
set cindent
set smartcase       " Case insensitive searches become sensitive with capitals

set lazyredraw
set showmode
set showcmd
set ruler
set ttyfast
set backspace=indent,eol,start
set laststatus=2
set history=10000
set undofile
set undoreload=10000
set nolist
set shell=/bin/bash
set autoread
set display=uhex
set nofsync
set shiftround
set notimeout
set nottimeout
set autowrite

" TODO: Consider
" > cindent, cinkeys, etc
" > set terminal size (columns, etc) in .gvimrc

set guioptions=aegimtr

" Consider highly-configuring 'guicursor'.

set completeopt=longest,menuone,preview " Review

" 	> Tabs __________________ {{{

set smarttab
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab
set wrap
set textwidth=85
set formatoptions=qrn1c

" 	}}}

" }}}
" Backups --------------------------------------------------------------------- {{{

set undodir=~/.tmp/vim/undo/      " undo files
set backupdir=~/.tmp/vim/backup/ " backups
set directory=~/.tmp/vim/swap/    " swap files
set backup                        " enabled

" }}}
" Status line ----------------------------------------------------------------- {{{

set statusline=%f    " Path.
set statusline+=%m   " Modified flag.
set statusline+=%r   " Readonly flag.
set statusline+=%w   " Preview window flag.

set statusline+=\    " Space.

"set statusline+=%#redbar#                " Highlight the following as a warning.
"set statusline+=%{SyntasticStatuslineFlag()} " Syntastic errors.
"set statusline+=%*                           " Reset highlighting.

set statusline+=%=   " Right align.

" File format, encoding and type.  Ex: "(unix/utf-8/python)"
set statusline+=(
set statusline+=%{&ff}                        " Format (unix/DOS).
set statusline+=/
set statusline+=%{strlen(&fenc)?&fenc:&enc}   " Encoding (utf-8).
set statusline+=/
set statusline+=%{&ft}                        " Type (python).
set statusline+=)

" Line and column position and counts.
set statusline+=\ (line\ %l\/%L,\ col\ %03c)

" }}}
" Searching and movement ------------------------------------------------------ {{{

" Use sane regexes.
nnoremap / /\v
vnoremap / /\v

set ignorecase
set smartcase
set incsearch
set showmatch
set hlsearch

set gdefault

set scrolloff=3
set sidescroll=1
set sidescrolloff=10

set virtualedit+=block

" }}}
" Folding --------------------------------------------------------------------- {{{

set foldlevelstart=0

function! MyFoldText() " {{{
    let line = getline(v:foldstart)

    let nucolwidth = &fdc + &number * &numberwidth
    let windowwidth = winwidth(0) - nucolwidth - 3
    let foldedlinecount = v:foldend - v:foldstart

    " expand tabs into spaces
    let onetab = strpart('          ', 0, &tabstop)
    let line = substitute(line, '\t', onetab, 'g')

    let line = strpart(line, 0, windowwidth - 2 -len(foldedlinecount))
    let fillcharcount = windowwidth - len(line) - len(foldedlinecount)
    return line . '…' . repeat(" ",fillcharcount) . foldedlinecount . '…' . ' '
endfunction " }}}
set foldtext=MyFoldText()
" }}}
" Filetype-specific stuff ----------------------------------------------------- {{{
"   > Javascript ____________ {{{
augroup ft_javascript
    au!

    au FileType javascript setlocal foldmethod=marker
    au FileType javascript setlocal foldmarker={,}
augroup END
"   }}}
"   > Python ________________ {{{
augroup ft_python
    au!

    au Filetype python noremap  <buffer> <localleader>rr :RopeRename<CR>
    au Filetype python noremap  <buffer> <localleader>ri :RopeOrganizeImports<CR>
    au Filetype python noremap  <buffer> <localleader>dd :RopeDefinition<CR>

    au FileType python setlocal omnifunc=pythoncomplete#Complete
    au Filetype python setlocal foldmethod=marker
augroup END
"   }}}
"   > QuickFix ______________ {{{
augroup ft_quickfix
    au!
    au Filetype qf setlocal colorcolumn=0 nolist nocursorline nowrap
augroup END
"   }}}
"   > Vim ___________________ {{{
augroup ft_vim
    au!

    au FileType vim setlocal foldmethod=marker
    au FileType help setlocal textwidth=78

    " TODO: Confirm what this does.
    au BufWinEnter *.txt if &ft == 'help' | wincmd L | endif
augroup END
"   }}}
"   > Mail __________________ {{{
augroup ft_mail
    au!

    au Filetype mail setlocal foldmethod=marker
augroup END
"   }}}
"   > Mako __________________ {{{
augroup ft_mako
    au!

    au BufRead,BufNewFile *.mako 	set filetype=mako

    au FileType mako setlocal foldmethod=indent
augroup END
"   }}}
"   > TeX ___________________ {{{
augroup ft_tex
    au!

    au BufRead,BufNewFile *.tex 	set filetype=tex
augroup END
"   }}}
" }}}
" Plugin configuration -------------------------------------------------------- {{{
"   > LaTeX-suite ___________ {{{
let g:Tex_DefaultTargetFormat = 'pdf'

" I prefer LaTeX to be built to /bin
let g:Tex_CompileRule_pdf = 'pdflatex -shell-escape -output-directory=bin -interaction=nonstopmode $*'
let g:Tex_ViewRule_pdf = 'okular'
let g:Tex_Debug = 0 " Tex_Debug Mode, if set to 1, use :call Tex_PrintDebug() to see the statements.
set grepprg=grep\ -nH\ $*
"   }}}
"   > Command-T _____________ {{{
let g:CommandTMaxHeight = 20

noremap <tab>e :CommandT<cr>

"   }}}
"   > EasyMotion ____________ {{{

let g:EasyMotion_leader_key = '.'

"   }}}
"   > Gundo _________________ {{{

noremap <leader>gu :GundoToggle<CR>
let g:gundo_preview_bottom = 1

"   }}}
"   > Supertab ______________ {{{

let g:SuperTabDefaultCompletionType = 'context'
let g:SuperTabLongestHighlight = 1

"   }}}
"   > TaskList ______________ {{{

"map <unique> <leader>tl <Plug>TaskList

"   }}}
"   > MiniBufExplorer _______ {{{

" NOTE: I've deleted MiniBufExplorer from my bundle,
" but may add it back at some point. I've decided tabs
" are the One True Path, with viewing buffers at my leisure
" via ":buffers"

" Single click to open buffer.

let g:miniBufExplUseSingleClick = 1

"   }}}
"   > ConqueTerm ____________ {{{

" Bugfix for ConqueTerm, doesn't check for this variable being
" defined.
let g:ConqueTerm_SessionSupport = 0

"   }}}
"   > VimWiki _______________ {{{

" Configure my wikis

let g:vimwiki_list = [{'path': '~/research/diary'}]

"   }}}
" }}}
" Quick editing of some typical files ----------------------------------------- {{{

nnoremap <leader>ev <C-w>s<C-w>j<C-w>L:e ~/.vimrc<cr>

" }}}
" Remappings ------------------------------------------------------------------ {{{
"   > General _______________ {{{

" r for repeat
noremap r .

" ConqueTerm
"noremap rr :ConqueTerm python<CR>
"noremap rbr :ConqueTerm bpython<CR>

noremap ' `
noremap ` <C-^>
noremap ; :

nnoremap <Space> za

" Keep search matches in the middle of the window.
nnoremap n nzzzv
nnoremap N Nzzzv

noremap <Home> <Esc>^

" 'op' pressed at the same time will do a paste
Arpeggioimap op <C-R>"

"   }}}
"   > Leaders _______________ {{{

let mapleader=','
let maplocalleader='\\' " TODO: Confirm what this is about.

map <leader>tl <Plug>VimwikiToggleListItem
map <leader>rw <Plug>VimwikiIndex

map <leader>u :call HandleURL()<CR>

" Window Navigation
noremap <Tab>h <C-w>h
noremap <Tab>j <C-w>j
noremap <Tab>k <C-w>k
noremap <Tab>l <C-w>l
noremap <Tab><Tab> <C-w>p

" Clear highlights from search
noremap <silent> <leader><space> :noh<cr>:call clearmatches()<cr>

" Compile and run LaTeX file in one step
map <leader>lp ,ll<CR>,lv<CR>

" Save the current session.
noremap <leader>s :mks! ~/.last_session.vim \| echo 'Session saved.'<CR>

" Open a Quickfix window for the last search.
nnoremap <silent> <leader>/ :execute 'vimgrep /'.@/.'/g %'<CR>:copen<CR>

" Open vimwiki

"   }}}
" }}}
" GUI configs ----------------------------------------------------------------- {{{

" Different cursors for different modes.
set guicursor=n-c:block-Cursor-blinkon0
set guicursor+=v:block-vCursor-blinkon0

" }}}
" Utility Functions ----------------------------------------------------------- {{{

" Stolen from SJL, stolen from https://github.com/askedrelic/homedir/blob/master/.vimrc
function! HandleURL()
    let s:uri = matchstr(getline("."), '[a-z]*:\/\/[^ >,;]*')
    echo s:uri
    if s:uri != ""
        exec "!firefox \"" . s:uri . "\""
    else
        echo "No URL found in line."
    endif
endfunction

" }}}
" Commands -------------------------------------------------------------------- {{{

" Find Todo's: Run's my todo util (http://github.com/silky/utils/find-todo) over
" some source dir, and posts the results in the quickfix window.
command! Gtta Gtt ~/dev
command! -nargs=1 -complete=file Gtt cgetexpr system('~/dev/silky-github/utils/find-todo/find-todo <args> concise') | copen

" }}}
