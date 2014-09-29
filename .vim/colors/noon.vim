" local syntax file - set colors on a per-machine basis:
" vim: tw=0 ts=4 sw=4
" Vim color file
" Based on "Pablo" by Ron Aaron <ron@ronware.org>
"
" Modified By:  Noon Silk <noonsilk@gmail.com>

hi clear
set background=dark
if exists("syntax_on")
  syntax reset
endif
let g:colors_name = "noon"

highlight Comment	 ctermfg=8						  guifg=#808080
highlight Constant	 ctermfg=14			   cterm=none guifg=#00dbdb				  gui=none
highlight Identifier ctermfg=6						  guifg=#de355f
highlight Statement  ctermfg=3			   cterm=bold guifg=#e3cb14				  gui=bold
highlight PreProc	 ctermfg=10						  guifg=#00ff00
highlight Type		 ctermfg=2						  guifg=#00c000
highlight Special	 ctermfg=12						                
highlight Error					ctermbg=9							guibg=#ff0000
highlight Todo		 ctermfg=4	ctermbg=3			  guifg=#000080 guibg=#c0c000
highlight Directory  ctermfg=2						  guifg=#00c000
highlight MatchParen guibg=black
highlight Conceal    guibg=#202020 guifg=#de355f

"highlight StatusLine ctermfg=11 ctermbg=12 cterm=none guifg=#ffff00 guibg=#e01b4c gui=none
highlight StatusLine                                  guibg=#0e0e0e guifg=#e01b4c gui=none
highlight StatusLineNC                                guibg=#d1d1d1 guifg=#0e0e0e
highlight VertSplit                                   guibg=black guifg=black

" For Seb, I've set the background to off-black.
highlight Normal									  guifg=#e1e1e1 guibg=#202020
highlight Search				ctermbg=3							guibg=#c0c000
highlight Visual guifg=White guibg=#c000bd ctermbg=12
highlight Folded ctermfg=216 guibg=#252525 guifg=#de355f

" This shows up in the pandoc markdown thing; disabling as it is annoying.
highlight FoldColumn guifg=#202020 guibg=#202020

" Todo: Fix this.
highlight Pmenu guifg=#e1e1e1 guibg=black
highlight PmenuSel guifg=black guibg=#e01b4c

highlight MyTagListFileName guifg=#e01b4c

" Highlighting for LaTeX. The various options can be found in
" the: </usr/share/vim/vim73/syntax/tex.vim> file.
"
" To see the current syntax settings type "hi".

highlight texMath guifg=#00dbdb
" Might be good for something: highlight texDef guibg=Pink
highlight texStatement guifg=#e01b4c
highlight texType guifg=Green
" texMathSymbol
"
highlight texGreek guifg=#00bf2e
"highlight texCmdArgs guibg=Purple
highlight texEnvBgn guibg=Purple


" MiniBufExplorer
"

highlight MBEVisibleChangedActive guifg=#e01b4c

" vimwiki
highlight Title gui=none guifg=#e4cb14
highlight Underlined guifg=#e01b63
