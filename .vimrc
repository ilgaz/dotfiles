call plug#begin()

Plug 'junegunn/goyo.vim'
Plug 'scrooloose/nerdtree' 
Plug 'vim-syntastic/syntastic'
Plug 'sheerun/vim-polyglot'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug '907th/vim-auto-save'
Plug 'mattn/emmet-vim'
Plug 'w0rp/ale'
Plug 'burner/vim-svelte'

call plug#end()

set number relativenumber
set encoding=utf-8
set wildmode=full
set splitbelow splitright
set tabstop=2
set softtabstop=0 
set noexpandtab
set shiftwidth=2
set autoindent
set smartindent

"utils
let g:airline_theme='kolor'
let g:solarized_termcolors=256 
let g:auto_save = 1
let g:auto_save_postsave_hook = 'ALEFix eslint'
let g:user_emmet_expandabbr_key = '\'
let g:user_emmet_install_global = 0
let g:ale_lint_on_save = 1
let g:ale_fix_on_save = 1
let b:ale_fixers = ['prettier', 'eslint']
syntax enable
set background=dark
colorscheme solarized

" stuff to auto-run on startup
autocmd VimEnter * NERDTree
autocmd VimEnter * :nohlsearch
autocmd FileType html,css EmmetInstall

"syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0





"ALE CONFIGS
let g:ale_linter_aliases = {'svelte': ['css', 'javascript', 'html']}
let g:ale_linters = {'svelte': ['prettier', 'eslint']}
