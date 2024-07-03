local t = require("telescope")
local builtin = require("telescope.builtin")
local actions = require("telescope.actions")
local action_layout = require("telescope.actions.layout")
local live_grep_args_shortcuts = require("telescope-live-grep-args.shortcuts")

vim.keymap.set('n', '<leader>p', builtin.find_files, {})
vim.keymap.set('n', 'gq', builtin.grep_string, {})
vim.keymap.set('n', '<leader>f', builtin.live_grep, {})
vim.keymap.set('n', '<leader>gb', builtin.buffers, {})
vim.keymap.set('n', '<leader>gs', function()
  builtin.grep_string({ search = vim.fn.input("Search: ") })
end)
vim.keymap.set('n', '<leader><leader>', "<Cmd>Telescope frecency workspace=CWD<CR>", {})

t.setup {
  defaults = {
    sorting_strategy = "ascending",
    layout_strategy = "flex",
    layout_config = {
      horizontal = { preview_cutoff = 80, preview_width = 0.55 },
      vertical = { mirror = true, preview_cutoff = 25 },
      prompt_position = "top",
      width = 0.8,
      height = 0.7,
    },
    file_ignore_patterns = { "node_modules", "dist" },
    mappings = {
      i = {
        ["<c-r>"] = "delete_buffer",
        ["<c-b>"] = action_layout.toggle_preview,
        ["<esc>"] = actions.close
      }
    },
    pickers = {
      buffers = {
        show_all_buffers = true,
        sort_lastused = true,
      }
    },
    extension = {
      frecency = {
        show_scores = false,
        ignore_patterns = { "*.git/*", "*/tmp/*", "*/node_modules/*", "*/Cellar/*" }
      }
    }
  }
}

t.load_extension("file_browser")

vim.api.nvim_set_keymap(
  "n",
  "<space>b",
  ":Telescope file_browser path=%:p:h select_buffer=true<CR>",
  { noremap = true }
)

