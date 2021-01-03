let &showbreak= '↪ '
let g:fzf_preview_window = 'right:85%'
let g:fzf_layout = { 'window': { 'width': 0.8, 'height': 0.8 }}
let $FZF_DEFAULT_OPS='--reverse'
let g:fzf_buffers_jump = 1
let mapleader=' '
let g:rg_highlight="true"
"let g:loaded_clipboard_provider = 1
let g:NERDTreeWinPos = "right"

let g:loaded_match_paren=1

if executable('rg')
    let g:rg_derive_root='true'
endif

let g:signify_realtime = 1
let g:indentLine_setColors = 0

let g:vimwiki_list = [{'path': '~/.vimwiki'}]
let g:completion_enable_snippet = 'UltiSnips'
