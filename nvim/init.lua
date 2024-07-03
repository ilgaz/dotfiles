require("ilgaz")

ts = require("nvim-treesitter")

vim.cmd[[colorscheme catppuccin-frappe]]
vim.o.termguicolors = true

vim.opt.tabstop = 2
vim.opt.shiftwidth = 2
vim.bo.softtabstop = 2
vim.opt.expandtab = true

vim.opt.foldmethod = "expr"
vim.opt.foldexpr = "nvim_treesitter#foldexpr()"
vim.opt.foldenable = false

vim.opt.updatetime = 100
vim.opt.signcolumn = "auto"

vim.wo.relativenumber = true

vim.opt.swapfile = false

-- function customise_terminal(color)
-- end

vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })

vim.cmd('autocmd BufRead,BufNewFile *.hbs set filetype=html')

-- customise_terminal()
