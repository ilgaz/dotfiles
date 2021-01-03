set number relativenumber
set encoding=utf-8
set wildmode=full
set splitbelow splitright
set tabstop=2
set softtabstop=0
set expandtab
set smarttab
set shiftwidth=2
set autoindent
set smartindent
set autoread
set noswapfile
set updatetime=100
set wrap
set linebreak
set autoread
set title
set nocompatible
set laststatus=0
set cmdheight=1
set noshowmatch
set completeopt=menuone,noinsert,noselect
set shortmess+=c

colorscheme ayu
let ayucolor="mirage"
syntax on
filetype plugin indent on

fun! TrimWhitespace()
    let l:save = winsaveview()
    keeppatterns %s/\s\+$//e
    call winrestview(l:save)
endfun

autocmd BufWritePre * :call TrimWhitespace()

command! -bang -nargs=* RG
  \ call fzf#vim#grep(
  \   'rg --column --line-number --no-heading --color=always --smart-case -- '.shellescape(<q-args>), 1,
  \   fzf#vim#with_preview(), <bang>0)

command! -bang -nargs=? -complete=dir Files
    \ call fzf#vim#files(<q-args>, fzf#vim#with_preview({'options': ['--layout=reverse', '--info=inline']}), <bang>0)

" LUA SETTINGS

lua require('telescope').setup({defaults = {file_sorter = require('telescope.sorters').get_fzy_sorter}})


lua require'lspconfig'.jsonls.setup{on_attach=require'completion'.on_attach}
lua require'lspconfig'.bashls.setup{on_attach=require'completion'.on_attach}
lua require'lspconfig'.tsserver.setup{on_attach=require'completion'.on_attach}
lua require'lspconfig'.rust_analyzer.setup{on_attach=require'completion'.on_attach}
let g:completion_matching_strategy_list = ['exact', 'substring', 'fuzzy']
