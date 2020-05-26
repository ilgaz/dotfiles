let g:rcpath = get(g:, 'rcpath', expand('~/Dotfiles/nvim/'))

exec 'source' g:rcpath.'plug.vim'
exec 'source' g:rcpath.'variables.vim'
exec 'source' g:rcpath.'colorscheme.vim'
exec 'source' g:rcpath.'coc.vim'
exec 'source' g:rcpath.'settings.vim'
exec 'source' g:rcpath.'remaps.vim'

autocmd CursorHold * silent call CocActionAsync('highlight')
autocmd VimEnter * :nohlsearch
"autocmd VimEnter * :NERDTreeToggle
autocmd FileType vim let b:vcm_tab_complete = 'vim'

augroup BgHighlight
    autocmd!
    autocmd WinEnter * set cul
    autocmd WinLeave * set nocul
augroup END

if (has("termguicolors"))
 set termguicolors
 let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
 let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
endif

highlight clear SignColumn
highlight GitGutterAdd ctermfg=green
highlight GitGutterChange ctermfg=yellow
highlight GitGutterDelete ctermfg=red
highlight GitGutterChangeDelete ctermfg=yellow
