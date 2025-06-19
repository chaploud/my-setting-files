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

-- ClojureではC-wの際に / . で止まって欲しい
vim.api.nvim_create_autocmd("FileType", {
  pattern = "clojure",
  callback = function()
    -- iskeywordから/と.を除外
    vim.opt_local.iskeyword:remove({"/", "."})
  end,
})

-- keymap: 'i': insert, 'n': normal, 'v': visual, 'c': command
-- Recenter Top Bottomをctrl+lにバインド
vim.keymap.set({'n', 'v', 'i'}, '<C-l>', recenter_cycle)

-- insertモードでのescapeを'fd'を素早く連続で押すことでも実行
vim.keymap.set('i', 'fd', '<ESC>')

-- insertモード時のEmacsキーバインド(移動・編集のみ)のエミュレート
vim.keymap.set('i', '<C-p>', '<Up>') -- ↑
vim.keymap.set('i', '<C-n>', '<Down>') -- ↓
vim.keymap.set('i', '<C-b>', '<Left>') -- ←
vim.keymap.set('i', '<C-f>', '<Right>') -- →
vim.keymap.set('i', '<C-a>', '<C-o>^') -- 行頭へ
vim.keymap.set('i', '<C-e>', '<End>') -- 行末へ
vim.keymap.set('i', '<C-d>', '<Del>') -- Del
vim.keymap.set('i', '<C-k>', '<C-o>d$') -- 行末まで削除
vim.keymap.set('i', '<C-y>', '<C-r>+') -- 直前のヤンクをペースト
vim.keymap.set({'i', 'c'}, '<C-h>', '<BS>') -- BackSpace

-- 選択状態でのペースト時にレジスタを上書きしない
vim.keymap.set('v', 'p', '"_dP')

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
  end)
else -- 通常のNeoVim利用時に有効
  -- ctrl+sでも保存
  vim.keymap.set({'n', 'i'}, '<C-s>', vim.cmd.write)
  -- ctrl+zでもundo
  vim.keymap.set({'n', 'i'}, '<C-z>', vim.cmd.undo)
  -- <leader> h でハイライトをオフ
  vim.keymap.set({'n', 'v'}, '<leader>h', vim.cmd.nohlsearch)
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
      -- (), [], {}, ""などの囲む系のテキストオブジェクトを追加
      "kylechui/nvim-surround",
      version = "^3.0.0",
      event = "VeryLazy",
      config = function()
        require("nvim-surround").setup({})
      end
    },
    {
      -- * による検索を改善
      "rapan931/lasterisk.nvim",
      event = "VeryLazy",
      config = function()
        vim.keymap.set('n', '*', function() require("lasterisk").search() end)
      end
    }
  },
  install = {},
  checker = { enabled = true },
})
