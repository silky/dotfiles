" Min latexbox stuff.

if exists('*fnameescape')
	function! s:FNameEscape(s)
		return fnameescape(a:s)
	endfunction
else
	function! s:FNameEscape(s)
		return a:s
	endfunction
endif

if !exists('b:LatexBox_loaded')

	let prefix = expand('<sfile>:p:h') . '/'

	execute 'source ' . s:FNameEscape(prefix . 'common.vim')
	execute 'source ' . s:FNameEscape(prefix . 'findmain.vim')
	execute 'source ' . s:FNameEscape(prefix . 'latexmk.vim')
	execute 'source ' . s:FNameEscape(prefix . 'mappings.vim')

	let b:LatexBox_loaded = 1
endif

" vim:fdm=marker:ff=unix:noet:ts=4:sw=4
