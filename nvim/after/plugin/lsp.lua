local lsp = require('lsp-zero').preset({})

lsp.ensure_installed({ "tsserver", "eslint" })

lsp.on_attach(function(client, bufnr)
  lsp.default_keymaps({buffer = buffer})
  
  vim.keymap.set("n", "gd", function() vim.lsp.buf.definition() end, opts)
  vim.keymap.set("n", "K", function() vim.lsp.buf.hover() end, opts)
  vim.keymap.set("n", "<leader>ca", function() vim.lsp.buf.code_action() end, opts)
  vim.keymap.set("n", "<C-k>", function() vim.lsp.buf.signature_help() end, opts)
end)

local cmp = require("cmp")
local cmp_select = {behavior = cmp.SelectBehavior.Select}
local cmp_mappings = lsp.defaults.cmp_mappings({
  ['<C-p'] = cmp.mapping.select_prev_item(cmp_select),
  ['<C-n'] = cmp.mapping.select_next_item(cmp_select),
  ['<C-b'] = cmp.mapping.scroll_docs(-4),
  ['<C-f'] = cmp.mapping.scroll_docs(4),
  ['<C-e'] = cmp.mapping.abort(),
  ['<CR>'] = cmp.mapping.confirm({ select = true }),
  ['<C-Space>'] = cmp.mapping.complete(),
  ['<Tab>'] = cmp.config.disable,
  ['<S-Tab>'] = cmp.config.disable,
})

lsp.set_preferences({
  suggest_lsp_servers = false,
  sign_icons = {
    error = 'E',
    warn = 'W',
    hint = 'H',
    info = 'I'
  }
})

lsp.setup_nvim_cmp({ mapping = cmp_mappings })

vim.diagnostic.config({ virtual_text = true })

lsp.setup()
