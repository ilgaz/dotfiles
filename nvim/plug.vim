call plug#begin("~/.vim/plugged")

Plug 'scrooloose/nerdtree'
Plug 'machakann/vim-highlightedyank'
Plug 'tpope/vim-fugitive'
Plug 'mhinz/vim-signify'
Plug 'pangloss/vim-javascript'
Plug 'jeetsukumaran/vim-buffergator'
Plug 'ap/vim-css-color'
Plug 'mbbill/undotree'
Plug 'jremmen/vim-ripgrep'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'vimwiki/vimwiki'
Plug 'junegunn/goyo.vim'
Plug 'tpope/vim-commentary'
Plug 'ayu-theme/ayu-vim'
Plug 'Yggdroot/indentLine'
Plug 'mxw/vim-jsx'
Plug 'szw/vim-maximizer'
Plug 'rust-lang/rust.vim'


" LUA
Plug 'nvim-lua/completion-nvim'
Plug 'neovim/nvim-lspconfig'
Plug 'nvim-lua/popup.nvim'
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-lua/telescope.nvim'
Plug 'tjdevries/lsp_extensions.nvim'


call plug#end()
