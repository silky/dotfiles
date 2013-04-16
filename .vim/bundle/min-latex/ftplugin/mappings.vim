" LaTeX Box mappings

if exists("g:LatexBox_no_mappings")
	finish
endif

" latexmk {{{
map <buffer> <LocalLeader>ll :Latexmk<CR>
map <buffer> <LocalLeader>lL :Latexmk!<CR>
map <buffer> <LocalLeader>lc :LatexmkClean<CR>
map <buffer> <LocalLeader>lC :LatexmkClean!<CR>
map <buffer> <LocalLeader>lg :LatexmkStatus<CR>
map <buffer> <LocalLeader>lG :LatexmkStatus!<CR>
map <buffer> <LocalLeader>lk :LatexmkStop<CR>
map <buffer> <LocalLeader>le :LatexErrors<CR>
" }}}

" View {{{
map <buffer> <LocalLeader>lv :LatexView<CR>
" }}}

