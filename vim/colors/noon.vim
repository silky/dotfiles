" vim: tw=0 ts=4 sw=4
" Vim color file
"
" By:  Noon Silk <noonsilk@gmail.com>

hi clear
set background=dark
if exists("syntax_on")
  syntax reset
endif
let g:colors_name = "noon"

" The normal background colour
highlight Normal                      ctermbg=236
highlight Comment	    ctermfg=206   
                                      
highlight StatusLine    ctermfg=175   ctermbg=17   cterm=none
highlight StatusLineNC  ctermfg=175   ctermbg=235  cterm=none
highlight VertSplit     ctermfg=17    ctermbg=17
                                      
highlight Todo		    ctermfg=216   ctermbg=236
highlight Visual        ctermfg=0     ctermbg=111
highlight Search	    ctermfg=0     ctermbg=209
highlight Statement     ctermfg=220
highlight Identifier    ctermfg=204                cterm=none
highlight MatchParen    ctermbg=233



highlight Constant	 ctermfg=81
highlight PreProc	 ctermfg=10
highlight Type		 ctermfg=2
highlight Special	 ctermfg=12
highlight Error					ctermbg=9							guibg=#ff0000

highlight Directory  ctermfg=2						  guifg=#00c000
highlight Conceal    guibg=#202020 guifg=#de355f


highlight Folded ctermfg=216 guibg=#252525 guifg=#de355f

" This shows up in the pandoc markdown thing; disabling as it is annoying.
highlight FoldColumn guifg=#202020 guibg=#202020

highlight Pmenu guifg=#e1e1e1 guibg=black
highlight PmenuSel guifg=black guibg=#e01b4c

highlight MyTagListFileName guifg=#e01b4c

" MiniBufExplorer

highlight MBEVisibleChangedActive guifg=#e01b4c

" vimwiki
highlight Title gui=none guifg=#e4cb14
highlight Underlined guifg=#e01b63


" Literate Haskell
highlight lhsBirdTrack guifg=#c000db
