" Noons .vimrc.
"
" Based on Steve Losh's .vimrc: http://bitbucket.org/sjl/dotfiles/src/tip/vim
"
" Author: Noon Silk <noonsilk@gmail.com>
" Location: https://github.com/silky/dotfiles/blob/master/.vimrc

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
set guifont=Akkurat-Mono\ 10

" }}}
" General Options ------------------------------------------------------------- {{{

set shortmess=at  " Ensure we don't get hit-enter prompts
set grepprg=grep\ -nH\ $*
set t_Co=256
set wildignore+=*.pyc,*.pdf,.git,.svn,*.png,*.jpg,*.ps,*.log,*.aux,*.out,*.dvi,*.fdb_latexmk
set winheight=30  " Autosize window to this height.

set encoding=utf-8
set modelines=1

" set cpoptions+=I "do NOT revert tabbing I have specifically set.
set cindent
set smartcase     " Case insensitive searches become sensitive with capitals

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
set foldlevelstart=1
set formatprg=fmt\ -w78

" cscope
"set cscopetagorder=1
"set cscopequickfix=s-,c-,d-,i-,t-,e-

set guioptions=aegit
set completeopt=longest,menuone,preview " TODO: Review

" 	> Tabs __________________ {{{

set smarttab
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab
set wrap
set textwidth=78
set formatoptions=qrjtco

" 	}}}

" }}}
" Backups --------------------------------------------------------------------- {{{

set undodir=~/.tmp/vim/undo/      " undo files
set backupdir=~/.tmp/vim/backup/  " backups
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

set foldlevelstart=99 " All folds open

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
"   > Pandoc  _______________ {{{
augroup ft_pandoc
    au!

    " Convert current file to pdf.
    au FileType pandoc noremap <buffer> <leader>ll :!pandoc -o "%.pdf" --template=a.latex "%"<cr>
    au FileType pandoc setlocal nocindent
    au FileType pandoc setlocal formatoptions=tcqron1
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
    au FileType vim setlocal foldlevel=0

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
    au FileType tex setlocal formatoptions=qrjtco

    " Set some common mappins I care about here; these are actually LaTeX-box
    " specific.

    " These seem slow maybe related to latexbox slowness in general.
    " au FileType tex imap <buffer> [[  \begin{
    " au FileType tex imap <buffer> [a  \begin{align*}
    " au FileType tex imap <buffer> ]]  <Plug>LatexCloseCurEnv
augroup END
"   }}}
" }}}
" Plugin configuration -------------------------------------------------------- {{{
"   > LaTeX-Box _____________ {{{
let g:LatexBox_latexmk_options = "-pvc -pdf"
" Folding is incredibly slow in latexbox, so we don't use it.
let g:LatexBox_Folding = 0
let g:LatexBox_fold_preamble = 0
let g:LatexBox_fold_envs = 0
"   }}}
"   > Command-T _____________ {{{
let g:CommandTMaxHeight = 20

noremap <tab>e :CommandT<cr>
noremap <tab>f :CommandTTag<cr>

let g:command_t_tag_include_filenames = 0

"   }}}
"   > EasyMotion ____________ {{{

let g:EasyMotion_leader_key = '.'

"   }}}
"   > Gundo _________________ {{{

noremap <F5> :GundoToggle<cr>
let g:gundo_preview_bottom = 1

"   }}}
"   > Supertab ______________ {{{

let g:SuperTabDefaultCompletionType = 'context'
let g:SuperTabLongestHighlight = 1

"   }}}
"   > TaskList ______________ {{{

"map <unique> <leader>tl <Plug>TaskList

"   }}}
"   > ConqueTerm ____________ {{{

" Bugfix for ConqueTerm, doesn't check for this variable being
" defined.
let g:ConqueTerm_SessionSupport = 0

"   }}}
"   > VimWiki _______________ {{{

" Configure my wikis

let g:vimwiki_list = [{'path': '~/research/diary'}, 
            \ {'path': '~/personal/notes'},
            \ {'path': '~/dev/otter/notes'}]

augroup ft_vimwiki
    au!

    " Remap next/prev as my tab key does other things.
    au Filetype vimwiki nmap <buffer> l <Plug>VimwikiNextLink
    au Filetype vimwiki nmap <buffer> L <Plug>VimwikiPrevLink
augroup END
"   }}}
"   > PyFlakes ______________ {{{

let g:pyflakes_use_quickfix = 0

"   }}}
"   > InlineEdit ____________ {{{

noremap <silent> <leader>e :InlineEdit<cr>

"   }}}
"   > Commentary ____________ {{{

" unmap \\\
nmap \c <Plug>CommentaryLine
vmap \c <Plug>Commentary

"   }}}
"   > Pep8 __________________ {{{
let g:pep8_map = '<F8>'
"   }}}
" }}}
" Remappings ------------------------------------------------------------------ {{{
"   > General _______________ {{{


" Making it so that scrolling with selections doen't screw up; we can find the
" previous selection again with 'gv'.

xnoremap <ScrollWheelUp> <esc><ScrollWheelUp>
xnoremap <ScrollWheelDown> <esc><ScrollWheelDown>


" r for repeat
noremap r .

" ConqueTerm
"noremap rr :ConqueTerm python<CR>
"noremap rbr :ConqueTerm bpython<CR>

noremap ' `
noremap ` <C-^>

" Faster way to get into command mode.
noremap ; :


" Shortcuts for saving
nnoremap <S-Space> :wa<cr>
nnoremap <Space> :w<cr>


" Keep search matches in the middle of the window.
nnoremap n nzzzv
nnoremap N Nzzzv

noremap <Home> <Esc>^

" Remap 'gf' to begin looking for the particular tag.
noremap gf :MyTag 

" Navigation keys
inoremap <Up> <NOP>
inoremap <Down> <NOP>
inoremap <Left> <NOP>
inoremap <Right> <NOP>
noremap <Up> <nop>
noremap <Down> <nop>
noremap <Left> <NOP>
noremap <Right> <NOP>

"   }}}
"   > Leaders _______________ {{{

let mapleader=','
let maplocalleader=',' 

" Finds tags, using 'AcK'. Defined below in the command section; will auto-complete
" to tags.
nmap <leader>t :FindAllTags 

" map <leader>tl <Plug>VimwikiToggleListItem
map <leader>rw <Plug>VimwikiIndex

" Close quickfix and preview windows, if open
map <silent> <leader>cw :ccl \| pc<cr>

map <silent> <leader>u :call HandleURL()<CR>

" Paste from general clipboard.
noremap <leader>p "+p

" Window Navigation
noremap <Tab>h <C-w>h
noremap <Tab>j <C-w>j
noremap <Tab>k <C-w>k
noremap <Tab>l <C-w>l
noremap <Tab><Tab> <C-w>p

" Clear highlights from search, also sets the last search to be empty.
noremap <silent> <leader><space> :noh<cr>:call clearmatches()<cr>

" Compile and run LaTeX file in one step
map <leader>lp ,ll<CR>,lv<CR>

" Save the current session.
noremap <leader>s :mks! ~/.last_session.vim \| echo 'Session saved.'<CR>

" Open a Quickfix window for the last search.
nnoremap <silent> <leader>/ :execute 'vimgrep /'.@/.'/g %'<CR>:copen<CR>

" Perform search in current directory for todos.
noremap <silent> <leader>gt :Gtt .<cr>

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



command! -nargs=1 -complete=tag FindAllTags :Ack '<args>'

" Find Todo's: Run's my todo util (http://github.com/silky/utils/find-todo) over
" some source dir, and posts the results in the quickfix window.
command! -nargs=1 -complete=file Gtt cgetexpr system('~/dev/silky-github/utils/find-todo/find-todo <args> concise') | copen

" Run a tag search and then focus the screen.
command! -nargs=1 -complete=tag MyTag :tag <args> | :normal zz

command! PyTags :DoTags *.py

" Generate a tags file and refresh CommandT; must pass the extensions you want.
command! -nargs=1 DoTags :call system("ctags -R <args> tags") | :CommandTFlush

" Regenerate the 'AutoGeneratedNotes' for my vimwiki
command! Agn :! cd ~/dev/utils/get-notes && ./get_notes.py && cd -

" }}}
" Wrapup ---------------------------------------------------------------------- {{{

" This does any things that don't need to be in the
" general config for all my machines; say setting font
" sizes.
source ~/.vimrc.local

" }}}
