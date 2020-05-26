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
set updatetime=100
set wrap
set linebreak
set autoread
set title
set nocompatible

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

syntax on
filetype plugin indent on

fun! TrimWhitespace()
    let l:save = winsaveview()
    keeppatterns %s/\s\+$//e
    call winrestview(l:save)
endfun

autocmd BufWritePre * :call TrimWhitespace()
