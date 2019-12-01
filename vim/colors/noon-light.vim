" vim: tw=0 ts=4 sw=4
" Vim color file
"
" By:  Noon Silk <noonsilk@gmail.com>
"
" https://jonasjacek.github.io/colors/

hi clear
set background=light
if exists("syntax_on")
  syntax reset
endif
let g:colors_name = "noon-light"

" The normal background colour
highlight Normal                      ctermbg=none

highlight Comment	    ctermfg=21
" highlight Comment	    ctermfg=165
highlight Statement     ctermfg=200
highlight Constant	    ctermfg=197
highlight Identifier    ctermfg=27                 cterm=none
highlight PreProc	    ctermfg=57
highlight Special	    ctermfg=243

highlight StatusLine    ctermfg=160   ctermbg=229  cterm=none
highlight StatusLineNC  ctermfg=160   ctermbg=250  cterm=none
highlight VertSplit     ctermfg=229   ctermbg=229

highlight Todo		    ctermfg=none  ctermbg=229
highlight Visual        ctermfg=0     ctermbg=111
highlight Search	    ctermfg=0     ctermbg=209
highlight MatchParen    ctermbg=229

highlight LineNr        ctermfg=246
highlight CursorLineNr  ctermfg=246



highlight Type		 ctermfg=93
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
