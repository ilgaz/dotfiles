call plug#begin("~/.vim/plugged")

Plug 'rust-lang/rust.vim'
Plug 'junegunn/goyo.vim'
Plug 'scrooloose/nerdtree'
Plug 'vim-syntastic/syntastic'
Plug 'sheerun/vim-polyglot'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug '907th/vim-auto-save'
Plug 'mattn/emmet-vim'
Plug 'w0rp/ale'
Plug 'justinmk/vim-sneak'
Plug 'machakann/vim-highlightedyank'
Plug 'cespare/vim-toml'
Plug 'stephpy/vim-yaml'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'tpope/vim-fugitive'
Plug 'dag/vim-fish'
"Plug 'ycm-core/YouCompleteMe'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'roxma/nvim-yarp'
Plug 'roxma/vim-hug-neovim-rpc'

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
set autoread
set noswapfile
set laststatus=2
set completeopt=noinsert,menuone,noselect
"utils
let g:airline_theme='deus'
let g:solarized_termcolors=256 
let g:auto_save = 1
let g:auto_save_postsave_hook = 'ALEFix eslint'
let g:user_emmet_expandabbr_key = '\'
let g:user_emmet_install_global = 0
let g:ale_lint_on_save = 1
let g:ale_fix_on_save = 1
let b:ale_fixers = ['prettier', 'eslint']
let g:deoplete#enable_at_startup = 1
syntax enable
filetype plugin indent on
compiler fish
setlocal foldmethod=expr

let g:ycm_server_python_interpreter = '/usr/bin/python'
let g:ycm_filepath_completion_use_working_dir=1
let g:rust_src_path = '/home/rsait/rustup_tmp/toolchains/nightly-x86_64-unknown-linux-gnu/'

" set background=dark
colorscheme dark_plus

" stuff to auto-run on startup
autocmd VimEnter * NERDTree
autocmd VimEnter * :nohlsearch
autocmd FileType html,css EmmetInstall
autocmd FileType vim let b:vcm_tab_complete = 'vim'

" Remaps
"nnoremap <silent> <tab> :if &modifiable && !&readonly && &modified <CR> :write<CR> :endif<CR>:bnext<CR>
"nnoremap <silent> <s-tab> :if &modifiable && !&readonly && &modified <CR> :write<CR> :endif<CR>:bprevious<CR>
nnoremap <silent> <C-b> :NERDTreeToggle<CR>

"syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

"Airline configs
let g:airline_powerline_fonts = 1
let g:airline_detect_modified = 1
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#formatter = 'unique_tail_improved'

"ALE CONFIGS
let g:ale_linter_aliases = {'svelte': ['css', 'javascript', 'html']}
let g:ale_linters = {'svelte': ['prettier']}

au Filetype rust source ~/.config/vim_scripts/spacetab.vim
