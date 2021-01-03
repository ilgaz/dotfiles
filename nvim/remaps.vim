nnoremap <silent> <C-t> :NERDTreeToggle<CR>
nnoremap <silent> <C-b> :BuffergatorToggle<CR>
nnoremap <silent> <C-l> :GitGutterLineHighlightsToggle<CR>
nmap <silent> <C-G> :G<CR>
nmap ghv <Plug>(GitGutterPreviewHunk)
vnoremap <leader>y "+y"
vnoremap J :m '>+1<CR>gv=gv
vnoremap K :m '<-2<CR>gv=gv
tnoremap <Esc> <C-\><C-n>
nnoremap <silent> <C-]> <C-^>
nmap ghn <Plug>(GitGutterNextHunk)
nmap ghp <Plug>(GitGutterPrevHunk)
nnoremap ghu :SignifyHunkUndo<CR>
nnoremap ghs :SignifyHunkDiff<CR>
nnoremap <C-w><space> :MaximizerToggle<CR>
nnoremap <leader>f :RG <C-R>=expand("<cword>")<CR><CR>

"LUA
inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
imap <silent> <c-space> <Plug>(completion_trigger)


nnoremap <leader>d :lua vim.lsp.buf.definition()<CR>
nnoremap <leader>i :lua vim.lsp.buf.implementation()<CR>
"nnoremap <leader>vsh :lua vim.lsp.buf.signature_help()<CR>
nnoremap <leader>n :lua vim.lsp.buf.references()<CR>
nnoremap <leader>r :lua vim.lsp.buf.rename()<CR>
nnoremap <leader>o :lua vim.lsp.buf.hover()<CR>
nnoremap <leader>ca :lua vim.lsp.buf.code_action()<CR>
nnoremap <leader>g :lua require('telescope.builtin').live_grep(require('telescope.themes').get_dropdown({}))<CR>
nnoremap <C-p> :lua require('telescope.builtin').git_files(require('telescope.themes').get_dropdown({}))<CR>
