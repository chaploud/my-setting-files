-- クリップボード共有
vim.opt.clipboard:append("unnamedplus")

-- ヤンクでハイライト
vim.api.nvim_create_autocmd("TextYankPost", {
  group = vim.api.nvim_create_augroup("YankHighlight", { clear = true }),
  callback = function()
    vim.highlight.on_yank({ timeout = 200 })
  end,
})

-- Recenter Top Bottom
local recenter_state = 0
local last_recenter_time = 0
local CONSECUTIVE_THRESHOLD = 3.0
local function recenter_cycle()
  local current_time = vim.fn.localtime()
  local time_diff = current_time - last_recenter_time
  if time_diff > CONSECUTIVE_THRESHOLD then
    recenter_state = 0
  end

  if recenter_state == 0 then
    vim.cmd("normal! zz")
    recenter_state = 1
  elseif recenter_state == 1 then
    vim.cmd("normal! zt")
    recenter_state = 2
  elseif recenter_state == 2 then
    vim.cmd("normal! zb")
    recenter_state = 0
  end

  last_recenter_time = current_time
end

vim.keymap.set({'n', 'v'}, '<C-l>', recenter_cycle, { noremap = true, silent = true })
vim.keymap.set('i', '<C-l>', recenter_cycle, { noremap = true, silent = true })

-- Emacsキーバインド
vim.cmd([[
  " Escape with fd
  " inoremap <silent> fd <ESC>

  " Save
  nnoremap <C-s> :w<CR>
  inoremap <C-s> <ESC>:w<CR>

  " Undo
  nnoremap <C-z> :undo<CR>
  inoremap <C-z> <ESC>:undo<CR>

  " Basic cursor movement and deletion keybindings from emacs, for vim.
  " insert mode
  imap <C-p> <Up>
  imap <C-n> <Down>
  imap <C-b> <Left>
  imap <C-f> <Right>
  imap <C-a> <C-o>^
  imap <C-e> <End>
  imap <C-d> <Del>
  imap <C-h> <BS>
  imap <C-k> <C-r>=<SID>kill_line()<CR>
  imap <C-y> <C-r>+
  imap <C-g> <ESC>

  " command line mode
  cmap <C-p> <Up>
  cmap <C-n> <Down>
  cmap <C-b> <Left>
  cmap <C-f> <Right>
  cmap <C-a> <Home>
  cmap <C-e> <End>
  cnoremap <C-d> <Del>
  cnoremap <C-h> <BS>
  cnoremap <C-k> <C-f>D<C-c><C-c>:<Up>
  cnoremap <C-g> <ESC>

  " command-T window
  let g:CommandTCursorLeftMap  = ['<Left>',  '<C-b>']
  let g:CommandTCursorRightMap = ['<Right>', '<C-f>']
  let g:CommandTBackspaceMap   = ['<BS>',    '<C-h>']
  let g:CommandTDeleteMap      = ['<Del>',   '<C-d>']

  xnoremap p "_dP

  function! s:kill_line()
    let current_line = getline('.')
    let current_col = col('.')
    let text_before_cursor = (current_col > 1) ? strpart(current_line, 0, current_col - 1) : ''
    let text_after_cursor = strpart(current_line, current_col - 1)
    if len(text_after_cursor) == 0
      let current_line_num = line('.')
      if current_line_num < line('$')
        call setreg('+', "\n", 'c')
	call setreg('"', "\n", 'c')
	execute 'normal! J'
      else
	return ''
      endif
    else
      call setline('.', text_before_cursor)
      call setreg('+', text_after_cursor, 'c')
      call setreg('"', text_after_cursor, 'c')
    endif
    return ''
  endfunction
]])

-- lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
  local lazyrepo = "https://github.com/folke/lazy.nvim.git"
  local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
  if vim.v.shell_error ~= 0 then
    vim.api.nvim_echo({
      { "Failed to clone lazy.nvim:\n", "ErrorMsg" },
      { out, "WarningMsg" },
      { "\nPress any key to exit..." },
    }, true, {})
    vim.fn.getchar()
    os.exit(1)
  end
end
vim.opt.rtp:prepend(lazypath)

vim.opt.ignorecase = true
vim.opt.smartcase = true

-- Setup lazy.nvim
require("lazy").setup({
  spec = {
    {
      -- カッコなどの包む系の操作を追加
      "kylechui/nvim-surround",
      version = "^3.0.0",
      event = "VeryLazy",
      config = function()
        require("nvim-surround").setup({})
      end
    }
  },
  install = {},
  checker = { enabled = false },
})
