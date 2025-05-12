-- Recenter Top Bottom
-- ctrl+l連打で、カーソル位置を画面の中央、上端、下端にシフトするサイクルを行う関数
local recenter_state = 0
local last_recenter_time = 0
-- 2秒間ctrl+lが押されなかったら、次回実行時は中央シフトからスタート
local CONSECUTIVE_THRESHOLD = 2000 -- ミリ秒
local function recenter_cycle()
  local current_time = vim.uv.now()
  local time_diff = current_time - last_recenter_time
  if time_diff > CONSECUTIVE_THRESHOLD then
    recenter_state = 0
  end

  if recenter_state == 0 then
    if vim.g.vscode then
      vim.fn.VSCodeExtensionNotify('reveal', 'center', 0)
    else
      vim.cmd("normal! zz")
    end
    recenter_state = 1
  elseif recenter_state == 1 then
    if vim.g.vscode then
      vim.fn.VSCodeExtensionNotify('reveal', 'top', 0)
    else
      vim.cmd("normal! zt")
    end
    recenter_state = 2
  elseif recenter_state == 2 then
    if vim.g.vscode then
      vim.fn.VSCodeExtensionNotify('reveal', 'bottom', 0)
    else
      vim.cmd("normal! zb")
    end
    recenter_state = 0
  end

  last_recenter_time = current_time
end

-- カーソルから行末までを削除し、レジスタに保存する関数 (コマンドラインモード用)
local function kill_line_cmd_expr()
  local cmdline_content = vim.fn.getcmdline()
  local cursor_pos = vim.fn.getcmdpos() -- 1-indexed byte position

  -- cmdline_content が nil または空、または cursor_pos が不正な場合のガード
  if not cmdline_content or #cmdline_content == 0 then
    return ""
  end
  if cursor_pos <= 0 then -- カーソル位置が先頭より前なら、先頭扱い
    cursor_pos = 1
  end
  if cursor_pos > #cmdline_content + 1 then -- カーソル位置が末尾より後ろなら、末尾+1扱い
     cursor_pos = #cmdline_content + 1
  end

  local text_before_cursor = string.sub(cmdline_content, 1, cursor_pos - 1)
  local text_after_cursor = string.sub(cmdline_content, cursor_pos)

  if text_after_cursor and #text_after_cursor > 0 then
    vim.fn.setreg('"', text_after_cursor) -- 無名レジスタに保存
    if vim.fn.has('clipboard') == 1 and vim.api.nvim_get_option_value('clipboard', {}):match('unnamedplus') then
      vim.fn.setreg('+', text_after_cursor) -- クリップボードにも保存
    end
    return text_before_cursor -- カーソル前のテキストを返す (コマンドラインがこれに置き換わる)
  else
    -- カーソルが行末にある場合は何も削除しない
    return cmdline_content -- 元のコマンドライン内容を返す
  end
end

-- クリップボードをOS側と共有
vim.opt.clipboard:append("unnamedplus")
-- 検索時に都合良く大文字小文字を区別／区別しない
vim.opt.ignorecase = true
vim.opt.smartcase = true

-- ヤンク(コピー)でハイライト
vim.api.nvim_create_autocmd("TextYankPost", {
  group = vim.api.nvim_create_augroup("YankHighlight", { clear = true }),
  callback = function()
    vim.highlight.on_yank({ timeout = 200 })
  end,
})
-- Recenter Top Bottomをctrl+lにバインド
vim.keymap.set({'n', 'v', 'i'}, '<C-l>', recenter_cycle, { noremap = true, silent = false })

-- insertモードでのescapeを'fd'を素早く連続で押すことでも実行
vim.keymap.set('i', 'fd', '<ESC>', { noremap = true, silent = false })

-- insert/command lineモード時のEmacsキーバインド(移動・編集のみ)のエミュレート
vim.keymap.set('i', '<C-p>', '<Up>', { noremap = true, silent = false }) -- ↑
vim.keymap.set('i', '<C-n>', '<Down>', { noremap = true, silent = false }) -- ↓
vim.keymap.set({'i', 'c'}, '<C-b>', '<Left>', { noremap = true, silent = false }) -- ←
vim.keymap.set({'i', 'c'}, '<C-f>', '<Right>', { noremap = true, silent = false }) -- →
vim.keymap.set('i', '<C-a>', '<C-o>^', { noremap = true, silent = false }) -- 挿入モード: 行頭へ
vim.keymap.set('c', '<C-a>', '<C-b>', { noremap = true, silent = false }) -- コマンドモード: 行頭へ
vim.keymap.set('i', '<C-e>', '<End>', { noremap = true, silent = false }) -- 行末へ
vim.keymap.set({'i', 'c'}, '<C-d>', '<Del>', { noremap = true, silent = false }) -- Del
vim.keymap.set({'i', 'c'}, '<C-h>', '<BS>', { noremap = true, silent = false }) -- BackSpace
-- コマンドラインモードの <C-k>: カーソルから行末まで削除
vim.keymap.set('c', '<C-k>', '<C-f>D<C-c><C-c>:<Up>', { expr = true, noremap = true, silent = false })
-- 挿入モードの <C-k>: カーソルから行末まで削除
vim.keymap.set('i', '<C-k>', '<C-o>d$', { noremap = true, silent = false })
vim.keymap.set({'i', 'c'}, '<C-y>', '<C-r>+', { noremap = true, silent = false }) -- 直前のヤンクをペースト

-- 選択状態でのペースト時にレジスタを上書きしない
vim.keymap.set('v', 'p', '"_dP', { noremap = true, silent = false })

-- <leader>キーをspaceに設定
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

if vim.g.vscode then -- VSCode NeoVimから呼び出された時に有効
  -- <leader>のディレイをなくす
  vim.o.timeout = false
  local vscode = require('vscode')
  -- <leader>キーがVSCode側で押された時、VSCode側のwhich-keyを起動
  vim.keymap.set({'n', 'v'}, '<leader>', function()
    vscode.call('whichkey.show')
  end, {noremap = true, silent = false})
else -- 通常のNeoVim利用時に有効
  vim.o.timeoutlen = 1000
  -- ctrl+sでも保存
  vim.keymap.set({'n', 'i'}, '<C-s>', vim.cmd.write, { noremap = true, silent = false })
  -- ctrl+zでもundo
  vim.keymap.set({'n', 'i'}, '<C-z>', vim.cmd.undo, { noremap = true, silent = false })
  -- <leader> h でハイライトをオフ
  vim.keymap.set({'n', 'v'}, '<leader>h', ':nohl<CR>', { noremap = true, silent = false })
end

-- lazy.nvimのインストール(初回のみ)
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

-- lazy.nvimの設定・プラグインの読み込み
require("lazy").setup({
  spec = {
    {
      -- カッコなどの包む系の操作を追加(nvim-surround)
      "kylechui/nvim-surround",
      version = "^3.0.0",
      event = "VeryLazy",
      config = function()
        require("nvim-surround").setup({})
      end
    }
  },
  install = {},
  checker = { enabled = true },
})