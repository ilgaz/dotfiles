vim.g.mapleader=" "
vim.keymap.set("n", "<leader>t", vim.cmd.Ex)

vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv")
vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv")

vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")

vim.keymap.set("x", "<leader>p", "\"_dP")
vim.keymap.set({"n", "v"}, "<leader>y", [["+y]])
vim.keymap.set({"n", "v"}, "<leader>y", [["+y]])

vim.keymap.set("i", "C-c", "<Esc>")
vim.keymap.set("n", "Q", "<nop>")

vim.keymap.set("n", "<C-k>", "<cmd>cnext<CR>zz")
vim.keymap.set("n", "<C-j>", "<cmd>cprev<CR>zz")
vim.keymap.set("n", "<leader>k", "<cmd>lnext<CR>zz")
vim.keymap.set("n", "<leader>j", "<cmd>lprev<CR>zz")

vim.keymap.set("n", "<leader>s", [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]])

vim.keymap.set("n", "<C-[>", function() vim.cmd("cprev") end)
vim.keymap.set("n", "<C-]>", function() vim.cmd("cnext") end)
vim.keymap.set("n", "<leader><C-[>", function() vim.cmd("colder") end)
vim.keymap.set("n", "<leader><C-]>", function() vim.cmd("cnewer") end)

vim.keymap.set("n", "<leader>d", function() vim.cmd("GitGutterLineHighlightsToggle") end)

