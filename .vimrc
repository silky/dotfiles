" Noons .vimrc.
"
" Based on Steve Losh's .vimrc: http://bitbucket.org/sjl/dotfiles/src/tip/vim
"
" Author: Noon Silk <noonsilk@gmail.com>
" Location: https://github.com/silky/dotfiles/blob/master/.vimrc


" New ----- {{{
" Use par for prettier line formatting
set formatprg="PARINIT='rTbgqR B=.,?_A_a Q=_s>|' par\ -w72"

" Use stylish haskell instead of par for haskell buffers
autocmd FileType haskell let &formatprg="stylish-haskell"

" Firstly define the leaders.
let mapleader=','
let maplocalleader=',' 


" Find custom built ghc-mod, codex etc
" let $PATH = $PATH . ':' . expand("~/.haskell-vim-now/bin")


" Set 7 lines to the cursor - when moving vertically using j/k
set so=7

" Turn on the WiLd menu
set wildmenu
" Tab-complete files up to longest unambiguous prefix
set wildmode=list:longest,full


" Show trailing whitespace
set list
" But only interesting whitespace
if &listchars ==# 'eol:$'
  set listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+
endif

" Configure backspace so it acts as it should act
set backspace=eol,start,indent
set whichwrap+=<,>,h,l

set ai   " Auto indent
set si   " Smart indent
set wrap " Wrap lines

" Pretty unicode haskell symbols
let g:haskell_conceal_wide = 1
let g:haskell_conceal_enumerations = 1

" Treat long lines as break lines (useful when moving around in them)
nnoremap j gj
nnoremap k gk


" Return to last edit position when opening files (You want this!)
augroup last_edit
  autocmd!
  autocmd BufReadPost *
       \ if line("'\"") > 0 && line("'\"") <= line("$") |
       \   exe "normal! g`\"" |
       \ endif
augroup END
" Remember info about open buffers on close
set viminfo^=%


" Slime {{{

vmap <silent> <Leader>rs <Plug>SendSelectionToTmux
nmap <silent> <Leader>rs <Plug>NormalModeSendToTmux
nmap <silent> <Leader>rv <Plug>SetTmuxVars

" }}}

" Alignment {{{

" Stop Align plugin from forcing its mappings on us
let g:loaded_AlignMapsPlugin=1
" Align on equal signs
map <Leader>a= :Align =<CR>
" Align on commas
map <Leader>a, :Align ,<CR>
" Align on pipes
map <Leader>a<bar> :Align <bar><CR>
" Prompt for align character
map <leader>ap :Align

" Enable some tabular presets for Haskell
let g:haskell_tabular = 1

" }}}

" Tags {{{

set tags=tags;/,codex.tags;/

let g:tagbar_type_haskell = {
    \ 'ctagsbin'  : 'hasktags',
    \ 'ctagsargs' : '-x -c -o-',
    \ 'kinds'     : [
        \  'm:modules:0:1',
        \  'd:data: 0:1',
        \  'd_gadt: data gadt:0:1',
        \  't:type names:0:1',
        \  'nt:new types:0:1',
        \  'c:classes:0:1',
        \  'cons:constructors:1:1',
        \  'c_gadt:constructor gadt:1:1',
        \  'c_a:constructor accessors:1:1',
        \  'ft:function types:1:1',
        \  'fi:function implementations:0:1',
        \  'o:others:0:1'
    \ ],
    \ 'sro'        : '.',
    \ 'kind2scope' : {
        \ 'm' : 'module',
        \ 'c' : 'class',
        \ 'd' : 'data',
        \ 't' : 'type'
    \ },
    \ 'scope2kind' : {
        \ 'module' : 'm',
        \ 'class'  : 'c',
        \ 'data'   : 'd',
        \ 'type'   : 't'
    \ }
\ }

" Generate haskell tags with codex and hscope
map <leader>tg :!codex update --force<CR>:call system("git hscope -X TemplateHaskell")<CR><CR>:call LoadHscope()<CR>

map <leader>tt :TagbarToggle<CR>

set csprg=~/.haskell-vim-now/bin/hscope
set csto=1 " search codex tags first
set cst
set csverb
nnoremap <silent> <C-\> :cs find c <C-R>=expand("<cword>")<CR><CR>
" Automatically make cscope connections
function! LoadHscope()
  let db = findfile("hscope.out", ".;")
  if (!empty(db))
    let path = strpart(db, 0, match(db, "/hscope.out$"))
    set nocscopeverbose " suppress 'duplicate connection' error
    exe "cs add " . db . " " . path
    set cscopeverbose
  endif
endfunction
au BufEnter /*.hs call LoadHscope()

" }}}

" Git {{{

let g:extradite_width = 60
" Hide messy Ggrep output and copen automatically
function! NonintrusiveGitGrep(term)
  execute "copen"
  " Map 't' to open selected item in new tab
  execute "nnoremap <silent> <buffer> t <C-W><CR><C-W>T"
  execute "silent! Ggrep " . a:term
  execute "redraw!"
endfunction

command! -nargs=1 GGrep call NonintrusiveGitGrep(<q-args>)
nmap <leader>gs :Gstatus<CR>
nmap <leader>gg :copen<CR>:GGrep 
nmap <leader>gl :Extradite!<CR>
nmap <leader>gd :Gdiff<CR>
nmap <leader>gb :Gblame<CR>

function! CommittedFiles()
  " Clear quickfix list
  let qf_list = []
  " Find files committed in HEAD
  let git_output = system("git diff-tree --no-commit-id --name-only -r HEAD\n")
  for committed_file in split(git_output, "\n")
    let qf_item = {'filename': committed_file}
    call add(qf_list, qf_item)
  endfor
  " Fill quickfix list with them
  call setqflist(qf_list, '')
endfunction

" Show list of last-committed files
nnoremap <silent> <leader>g? :call CommittedFiles()<CR>:copen<CR>

" }}}

" Haskell Interrogation {{{

set completeopt+=longest

" Use buffer words as default tab completion
let g:SuperTabDefaultCompletionType = '<c-x><c-p>'

" But provide (neco-ghc) omnicompletion
if has("gui_running")
  imap <c-space> <c-r>=SuperTabAlternateCompletion("\<lt>c-x>\<lt>c-o>")<cr>
else " no gui
  if has("unix")
    inoremap <Nul> <c-r>=SuperTabAlternateCompletion("\<lt>c-x>\<lt>c-o>")<cr>
  endif
endif

" Show types in completion suggestions
let g:necoghc_enable_detailed_browse = 1

" Type of expression under cursor
nmap <silent> <leader>ht :GhcModType<CR>
" Insert type of expression under cursor
nmap <silent> <leader>hT :GhcModTypeInsert<CR>
" GHC errors and warnings
nmap <silent> <leader>hc :SyntasticCheck ghc_mod<CR>
" Haskell Lint
let g:syntastic_mode_map = { 'mode': 'active', 'passive_filetypes': ['haskell'] }
nmap <silent> <leader>hl :SyntasticCheck hlint<CR>

" Hoogle the word under the cursor
nnoremap <silent> <leader>hh :Hoogle<CR>

" Hoogle and prompt for input
nnoremap <leader>hH :Hoogle 

" Hoogle for detailed documentation (e.g. "Functor")
nnoremap <silent> <leader>hi :HoogleInfo<CR>

" Hoogle for detailed documentation and prompt for input
nnoremap <leader>hI :HoogleInfo 

" Hoogle, close the Hoogle window
nnoremap <silent> <leader>hz :HoogleClose<CR>

" }}}

" Conversion {{{

function! Pointfree()
  call setline('.', split(system('pointfree '.shellescape(join(getline(a:firstline, a:lastline), "\n"))), "\n"))
endfunction
vnoremap <silent> <leader>h. :call Pointfree()<CR>

function! Pointful()
  call setline('.', split(system('pointful '.shellescape(join(getline(a:firstline, a:lastline), "\n"))), "\n"))
endfunction
vnoremap <silent> <leader>h> :call Pointful()<CR>

" }}}


" }}}


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
" set guifont=Akkurat-Mono\ 10
set guifont=Droid\ Sans\ Mono\ 10
" set guifont=Hasklig\ Medium\ 10

" }}}
" General Options ------------------------------------------------------------- {{{

set mouse=a
set shortmess=at  " Ensure we don't get hit-enter prompts
set grepprg=grep\ -nH\ $*
set t_Co=256
set wildignore+=*.pyc,*.pdf,.git,.svn,*.png,*.jpg,*.ps,*.log,*.aux,*.out,*.dvi,*.fdb_latexmk,*.pyo
set wildignore+=*.lib,*.exe,*.dll,*.pdb,*.exp
set wildignore+=*.dat,*.id,*.map,*.DAT,*.ID,*.MAP,*.tab,*.TAB,*.mid,*.mif,*.MID,*.MIF
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
set shell=/bin/zsh
set autoread
set display=uhex
set nofsync
set shiftround
set notimeout
set nottimeout
set autowrite

" Folds
set foldmethod=indent
set foldnestmax=5
set foldlevelstart=99
set foldcolumn=0

" set formatprg=fmt\ -w78

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

set noswapfile
set undodir=~/.tmp/vim/undo/      " undo files
set backupdir=~/.tmp/vim/backup/  " backups
set directory=~/.tmp/vim/swap/    " swap files
set nobackup                      " disabled
set nowb

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
    au Filetype python setlocal foldmethod=indent
    au Filetype python setlocal foldnestmax=2
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
    au FileType tex imap <buffer> [ba \begin{align*}
    au FileType tex imap <buffer> [ea \end{align*}
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
"   > VimWiki _______________ {{{

" Configure my wikis

let g:vimwiki_list = [{'path': '~/research/diary'}, 
            \ {'path': '~/personal/notes'},
            \ {'path': '~/dev/otter/notes'}]

augroup ft_vimwiki
    au!

    " Remap next/prev as my tab key does other things.
    " au Filetype vimwiki nmap <buffer> l <Plug>VimwikiNextLink
    " au Filetype vimwiki nmap <buffer> L <Plug>VimwikiPrevLink
augroup END
"   }}}
"   > PyFlakes ______________ {{{

let g:pyflakes_use_quickfix = 0

"   }}}
"   > Commentary ____________ {{{

" unmap \\\
nmap \c <Plug>CommentaryLine
vmap \c <Plug>Commentary

autocmd FileType cabal setlocal commentstring=--\ %s

"   }}}
"   > Pep8 __________________ {{{
let g:pep8_map = '<F8>'
"   }}}
"   > Vimux _________________ {{{
function! VimuxSendToGhci()
    call VimuxSendText(":{\n")
    call VimuxSlime()
    call VimuxSendText(":}\n")
endfunction

function! VimuxSlime()
    call VimuxSendText(@v)
    call VimuxSendKeys("Enter")
endfunction

"   }}}
" }}}
" Remappings ------------------------------------------------------------------ {{{
"   > General _______________ {{{


" Making it so that scrolling with selections doen't screw up; we can find the
" previous selection again with 'gv'.

" xnoremap <ScrollWheelUp> <esc><ScrollWheelUp>
" xnoremap <ScrollWheelDown> <esc><ScrollWheelDown>

" Diable man lookup
nnoremap K <nop>
vnoremap K <nop>

vnoremap r "_dP

" r for repeat
nnoremap r .

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
inoremap <Up>    <nop>
inoremap <Down>  <nop>
inoremap <Left>  <nop>
inoremap <Right> <nop>
noremap <Up>     <nop>
noremap <Down>   <nop>
noremap <Left>   <nop>
noremap <Right>  <nop>

"   }}}
"   > Leaders _______________ {{{

" Finds tags, using 'AcK'. Defined below in the command section; will auto-complete
" to tags.
nmap <leader>t :FindAllTags 

" map <leader>tl <Plug>VimwikiToggleListItem
map <leader>rw <Plug>VimwikiIndex

" Close quickfix and preview windows, if open
map <silent> <leader>cw :ccl \| pc<cr>

map <silent> <leader>u :call HandleURL()<CR>


" vimtmux thingss
vmap <silent> <leader>hh "vy :call VimuxSendToGhci()<cr>
nmap <silent> <leader>ghc :call VimuxRunCommand("ghci-colour")<cr>
" If text is selected, save it in the v buffer and send that buffer it to tmux
vmap <leader>vs "vy :call VimuxSlime()<cr>

" Select current paragraph and send it to tmux
nmap <silent> <leader>vs vip<LocalLeader>vs<cr>

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
command! -nargs=1 -complete=file Gtt cgetexpr system('~/dev/utils/find-todo/find-todo <args> concise') | copen

" Run a tag search and then focus the screen.
command! -nargs=1 -complete=tag MyTag :tag <args> | :normal zz

command! PyTags :DoTags **/*.py

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



let s:pattern = '^\(.* \)\([1-9][0-9]*\)$'
let s:minfontsize = 6
let s:maxfontsize = 16
function! AdjustFontSize(amount)
  if has("gui_gtk2") && has("gui_running")
    let fontname = substitute(&guifont, s:pattern, '\1', '')
    let cursize = substitute(&guifont, s:pattern, '\2', '')
    let newsize = cursize + a:amount
    if (newsize >= s:minfontsize) && (newsize <= s:maxfontsize)
      let newfont = fontname . newsize
      let &guifont = newfont
    endif
  else
    echoerr "You need to run the GTK2 version of Vim to use this function."
  endif
endfunction

function! LargerFont()
  call AdjustFontSize(1)
endfunction
command! LargerFont call LargerFont()

function! SmallerFont()
  call AdjustFontSize(-1)
endfunction
command! SmallerFont call SmallerFont()

nmap <M--> :call SmallerFont()<cr>
nmap <M-=> :call LargerFont()<cr>
