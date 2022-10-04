" Noons new vim configuration file for NeoVim.
let g:ormolu_disable=1
let g:ormolu_command="fourmolu"
let g:ormolu_suppress_stderr=1

" nnoremap ff :call RunOrmolu()<CR>
xnoremap ff :<c-u>call OrmoluBlock()<CR>


let $FZF_DEFAULT_COMMAND = 'rg --files -M 1000'
let g:fzf_preview_window = ''

" Firstly define the leaders.
let mapleader=','
let maplocalleader=','

" Use par for prettier line formatting
set formatprg="PARINIT='rTbgqR B=.,?_A_a Q=_s>|' par\ -w72"

set nocompatible
syntax on
colorscheme noon-light


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

" Remove trailing whitespace on save
fun! <SID>StripTrailingWhitespaces()
    let l = line(".")
    let c = col(".")
    %s/\s\+$//e
    call cursor(l, c)
endfun

autocmd BufWritePre * :call <SID>StripTrailingWhitespaces()

" Configure backspace so it acts as it should act
set backspace=eol,start,indent
set whichwrap+=<,>,h,l

" https://www.hillelwayne.com/post/intermediate-vim/
set inccommand=nosplit

set ai   " Auto indent
set si   " Smart indent
set wrap " Wrap lines


" Treat long lines as break lines
nnoremap j gj
nnoremap k gk


" Return to last edit position when opening files
augroup last_edit
  autocmd!
  autocmd BufReadPost *
       \ if line("'\"") > 0 && line("'\"") <= line("$") |
       \   exe "normal! g`\"" |
       \ endif
augroup END
" Remember info about open buffers on close
set viminfo^=%

" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)



" Enable some tabular presets for Haskell
let g:haskell_tabular = 1



set mouse=a
set shortmess=at  " Ensure we don't get hit-enter prompts
set grepprg=grep\ -nH\ $*
set t_Co=256
set wildignore+=*.pyc,*.pdf,.git,.svn,*.png,*.jpg,*.ps,*.log,*.aux,*.out,*.dvi,*.fdb_latexmk,*.pyo
set wildignore+=*.lib,*.exe,*.dll,*.pdb,*.exp
set winheight=30  " Autosize window to this height.
set encoding=utf-8
set modelines=1

set cpoptions+=I "do NOT revert tabbing I have specifically set.
set cindent
set smartcase     " Case insensitive searches become sensitive with capitals
set lazyredraw
set showmode
set showcmd
set ruler
set ttyfast
set laststatus=2
set history=10000
set undofile
set undoreload=10000
set nolist
set shell=zsh
set autoread
set display=uhex
set nofsync
set shiftround
set notimeout
set nottimeout
set autowrite


set guioptions=aegit
set completeopt=longest,menuone,preview " TODO: Review
set smarttab
set tabstop=2
set shiftwidth=2
set softtabstop=2
set expandtab
set wrap
set textwidth=78
set formatoptions=qrjtco


set noswapfile
set undodir=~/.tmp/vim/undo/      " undo files
set backupdir=~/.tmp/vim/backup/  " backups
set directory=~/.tmp/vim/swap/    " swap files
set nobackup                      " disabled
set nowb
set nocursorcolumn
set nocursorline
set rnu

let $NVIM_TUI_ENABLE_CURSOR_SHAPE = 0
set guicursor=

let g:clipboard = {
  \   'name': 'helic',
  \   'copy': {
  \      '+': 'hel yank --agent nvim',
  \      '*': 'hel yank --agent nvim',
  \    },
  \   'paste': {
  \      '+': 'xsel -bo',
  \      '*': 'xsel -bo',
  \   },
  \ }

" Status line -----------------------------------------------------------------

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


" Use sane regexes.
nnoremap / /\v
vnoremap / /\v

set ignorecase
set smartcase
set incsearch
set hlsearch
set noshowmatch

set gdefault

set scrolloff=3
set sidescroll=1
set sidescrolloff=10

set virtualedit+=block


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

noremap YY :%y+<cr>


" Shortcuts for saving
nnoremap <S-Space> :wa<cr>
nnoremap <Space> :w<cr>


" Keep search matches in the middle of the window.
nnoremap n nzzzv
nnoremap N Nzzzv

noremap <Home> <Esc>^



" Navigation keys
inoremap <Up>    <nop>
inoremap <Down>  <nop>
inoremap <Left>  <nop>
inoremap <Right> <nop>
noremap <Up>     <nop>
noremap <Down>   <nop>
noremap <Left>   <nop>
noremap <Right>  <nop>


" Paste from general clipboard.
noremap <leader>p "+p
" Yank to the general clipboard.
noremap <leader>y "+y

" set termguicolors
set clipboard+=unnamed

" Mapping selecting mappings
nmap <Tab>e :Files<cr>
nmap <Tab>s :GFiles?<cr>
" TODO: How to make this work with input()/<expr> ?
" nmap <Tab>l :Lines <cr>

" Window Navigation
noremap <Tab>h <C-w>h
noremap <Tab>j <C-w>j
noremap <Tab>k <C-w>k
noremap <Tab>l <C-w>l
noremap <Tab><Tab> <C-w>p

" Clear highlights from search, also sets the last search to be empty.
noremap <silent> <leader><space> :noh<cr>:call clearmatches()<cr>


function! SynStack()
  if !exists("*synstack")
    return
  endif
  echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunc


let g:matchparen_timeout = 10
let g:matchparen_insert_timeout = 10


set timeoutlen=1000 ttimeoutlen=0



nmap \\  <Plug>CommentaryLine
vmap \\  <Plug>Commentary


" EasyMotion
let g:EasyMotion_leader_key = '.'


augroup readonly
  au!
  au BufEnter * if(!&modifiable || &ro) | :highlight Normal ctermbg=255 | endif
augroup end


" Pandoc
augroup ft_pandoc
    au!

    " Convert current file to pdf.
    au FileType pandoc noremap <buffer> <leader>ll :!pandoc -o "%.pdf" --template=a.latex "%"<cr>
    au FileType pandoc setlocal nocindent
    au FileType pandoc setlocal formatoptions=tcqron1
augroup END

let g:elm_format_autosave = 0

" https://ro-che.info/articles/2020-07-08-integrate-ghcid-vim
set errorformat=%C%*\\s•\ %m,
               \%-C\ %.%#,
               \%A%f:%l:%c:\ %t%.%#





"" Coc-Vim
"" COC {{{
"" Give more space for displaying messages.
"" set cmdheight=2
"" Having longer updatetime (default is 4000 ms = 4 s) leads to noticeable
"" delays and poor user experience.
"set updatetime=300
"" Don't pass messages to |ins-completion-menu|.
"set shortmess+=c
"" Always show the signcolumn, otherwise it would shift the text each time
"" diagnostics appear/become resolved.
"if has("nvim-0.5.0") || has("patch-8.1.1564")
"  " Recently vim can merge signcolumn and number column into one
"  set signcolumn=number
"else
"  set signcolumn=yes
"endif
"" Use tab for trigger completion with characters ahead and navigate.
"" NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
"" other plugin before putting this into your config.
"" inoremap <silent><expr> <TAB>
""       \ pumvisible() ? "\<C-n>" :
""       \ <SID>check_back_space() ? "\<TAB>" :
""       \ coc#refresh()
"" inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"
"" function! s:check_back_space() abort
""   let col = col('.') - 1
""   return !col || getline('.')[col - 1]  =~# '\s'
"" endfunction
"" Use <c-space> to trigger completion.
"if has('nvim')
"  inoremap <silent><expr> <c-space> coc#refresh()
"else
"  inoremap <silent><expr> <c-@> coc#refresh()
"endif
"" Make <CR> auto-select the first completion item and notify coc.nvim to
"" format on enter, <cr> could be remapped by other vim plugin
"inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm()
"                              \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"
"" Use `[g` and `]g` to navigate diagnostics
"" Use `:CocDiagnostics` to get all diagnostics of current buffer in location list.
"nmap <silent> [g <Plug>(coc-diagnostic-prev)
"nmap <silent> ]g <Plug>(coc-diagnostic-next)
"" GoTo code navigation.
"nmap <silent> gd <Plug>(coc-definition)
"nmap <silent> gy <Plug>(coc-type-definition)
"nmap <silent> gi <Plug>(coc-implementation)
"nmap <silent> gr <Plug>(coc-references)
"" Use K to show documentation in preview window.
"nnoremap <silent> K :call <SID>show_documentation()<CR>
"function! s:show_documentation()
"  if (index(['vim','help'], &filetype) >= 0)
"    execute 'h '.expand('<cword>')
"  elseif (coc#rpc#ready())
"    call CocActionAsync('doHover')
"  else
"    execute '!' . &keywordprg . " " . expand('<cword>')
"  endif
"endfunction
"" Highlight the symbol and its references when holding the cursor.
"" autocmd CursorHold * silent call CocActionAsync('highlight')
"" Symbol renaming.
"nmap <leader>rn <Plug>(coc-rename)
"" Formatting selected code.
"xmap <leader>gf <Plug>(coc-format-selected)
"nmap <leader>gf <Plug>(coc-format-selected)
"augroup mygroup
"  autocmd!
"  " Setup formatexpr specified filetype(s).
"  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
"  " Update signature help on jump placeholder.
"  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
"augroup end
"" Applying codeAction to the selected region.
"" Example: `<leader>aap` for current paragraph
"xmap <leader>a  <Plug>(coc-codeaction-selected)
"nmap <leader>a  <Plug>(coc-codeaction-selected)
"" Remap keys for applying codeAction to the current buffer.
"nmap <leader>ac  <Plug>(coc-codeaction)
"" Apply AutoFix to problem on the current line.
"nmap <leader>qf  <Plug>(coc-fix-current)
"" Map function and class text objects
"" NOTE: Requires 'textDocument.documentSymbol' support from the language server.
"xmap if <Plug>(coc-funcobj-i)
"omap if <Plug>(coc-funcobj-i)
"xmap af <Plug>(coc-funcobj-a)
"omap af <Plug>(coc-funcobj-a)
"xmap ic <Plug>(coc-classobj-i)
"omap ic <Plug>(coc-classobj-i)
"xmap ac <Plug>(coc-classobj-a)
"omap ac <Plug>(coc-classobj-a)
"" Remap <C-f> and <C-b> for scroll float windows/popups.
"if has('nvim-0.4.0') || has('patch-8.2.0750')
"  nnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
"  nnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
"  inoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(1)\<cr>" : "\<Right>"
"  inoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(0)\<cr>" : "\<Left>"
"  vnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
"  vnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
"endif
"" Use CTRL-S for selections ranges.
"" Requires 'textDocument/selectionRange' support of language server.
"" nmap <silent> <C-s> <Plug>(coc-range-select)
"" xmap <silent> <C-s> <Plug>(coc-range-select)
"" Add `:Format` command to format current buffer.
"command! -nargs=0 Format :call CocAction('format')
"" Add `:Fold` command to fold current buffer.
"command! -nargs=? Fold :call     CocAction('fold', <f-args>)
"" Add `:OR` command for organize imports of the current buffer.
"command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')
"" Add (Neo)Vim's native statusline support.
"" NOTE: Please see `:h coc-status` for integrations with external plugins that
"" provide custom statusline: lightline.vim, vim-airline.
"" set statusline^=%{coc#status()} %{get(b:,'coc_current_function','')}
"" Mappings for CoCList
"" Show all diagnostics.
"nnoremap <silent><nowait> <space>a  :<C-u>CocList diagnostics<cr>
"" Manage extensions.
"nnoremap <silent><nowait> <space>e  :<C-u>CocList extensions<cr>
"" Show commands.
"nnoremap <silent><nowait> <space>c  :<C-u>CocList commands<cr>
"" Find symbol of current document.
"nnoremap <silent><nowait> <space>o  :<C-u>CocList outline<cr>
"" Search workspace symbols.
"nnoremap <silent><nowait> <space>s  :<C-u>CocList -I symbols<cr>
"" Do default action for next item.
"nnoremap <silent><nowait> <space>j  :<C-u>CocNext<CR>
"" Do default action for previous item.
"nnoremap <silent><nowait> <space>k  :<C-u>CocPrev<CR>
"" Resume latest coc list.
"nnoremap <silent><nowait> <space>p  :<C-u>CocListResume<CR>
""}}}












