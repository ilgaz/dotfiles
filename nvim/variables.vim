let &showbreak= '↪ '
let g:fzf_preview_window = 'right:55%'
let g:ale_linters = {
\   'typescript': ['eslint', 'tsserver', 'typecheck'],
\}

" Highlight symbol under cursor on CursorHold
"utils
let mapleader=' '
let g:airline_theme='material'
let g:solarized_termcolors=256
let g:ale_lint_on_save = 1
let g:ale_fix_on_save = 1
let g:ale_fixers = {
\  'javascript': ['eslint'],
\  'jsx': ['eslint']
\}
let g:ale_linters = {
\  'javascript': ['eslint'],
\  'jsx': ['eslint']
\}
let g:ale_sign_error = '✘'
let g:ale_sign_warning = '⚠'
let g:ale_javascript_prettier_options = '--no-semi --single-quote --trailing-comma none'
let g:rg_highlight="true"
let g:rg_derive_root="true"

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

"Airline configs
let g:airline_powerline_fonts = 1
let g:airline_detect_modified = 1
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#formatter = 'unique_tail_improved'

