-- Recenter Top Bottom
-- ctrl+l連打で、カーソル位置を画面の中央、上端、下端にシフトするサイクルを行う関数
-- 2秒間ctrl+lが押されなかったら、次回実行時は中央シフトからスタート
local recenter_state = 0
local last_recenter_time = 0
local CONSECUTIVE_THRESHOLD = 2000 -- ミリ秒

local function recenter_cycle()
  -- 一定時間経過でリセット
  local current_time = vim.uv.now()
  if current_time - last_recenter_time > CONSECUTIVE_THRESHOLD then
    recenter_state = 0
  end

  -- 配列で位置とコマンドを定義
  local positions = {
    { pos = "center", cmd = "zz" },
    { pos = "top", cmd = "zt" },
    { pos = "bottom", cmd = "zb" }
  }

  -- 現在の状態に対応する位置を取得
  local current = positions[recenter_state + 1]

  -- VSCodeかNeovimネイティブかによって適切な方法で実行
  if vim.g.vscode then
    vim.fn.VSCodeExtensionNotify('reveal', current.pos, 0)
  else
    vim.cmd("normal! " .. current.cmd)
  end

  -- 状態を更新（0→1→2→0→...）
  recenter_state = (recenter_state + 1) % 3
  last_recenter_time = current_time
end

-- vscode.call
local vscode
local function call_vscode(name, ...)
  if vim.g.vscode then
    if not vscode then
      vscode = require('vscode')
    end
    return vscode.call(name, ...)
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

-- ctrl+sでも保存
vim.keymap.set({'n', 'i'}, '<C-s>', vim.cmd.write)
-- ctrl+zでもundo
vim.keymap.set({'n', 'i'}, '<C-z>', vim.cmd.undo)

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
    },
    {
      -- which-key
      "folke/which-key.nvim",
      event = "VeryLazy",
      opts = {
        delay = 0,
        show_help = false,
        show_keys = false,
        icons = {
          mappings = false
        }
      },
      keys = {
        {
          "<leader>h",
          vim.cmd.nohlsearch,
          desc = "Highlights Off"
        },
        {
          "<leader>f",
          function() call_vscode('workbench.view.explorer') end,
          desc = "File Explorer"
        },
        {
          "<leader>s",
          function() call_vscode('workbench.view.search') end,
          desc = "Search in Project"
        },
        {
          "<leader>r",
          function() call_vscode('editor.action.rename') end,
          desc = "Rename Symbol"
        },
        {
          "<leader>i",
          function() call_vscode('editor.action.goToImplementation') end,
          desc = "Go to Implementation"
        },
        {
          "<leader>e",
          function() call_vscode('code-runner.run') end,
          desc = "Execute File"
        },
        {
          "<leader>n",
          function() call_vscode('editor.action.marker.next') end,
          desc = "Next Marker"
        },
        {
          "<leader>p",
          function() call_vscode('editor.action.marker.prev') end,
          desc = "Previous Marker"
        },
        {
          "<leader>d",
          function() call_vscode('workbench.action.debug.start') end,
          desc = "Debug Start"
        },
        {
          "<leader>D",
          function() call_vscode('workbench.action.debug.run') end,
          desc = "Debug Run"
        },
        {
          "<leader>'",
          function() call_vscode('workbench.action.createTerminalEditorSide') end,
          desc = "Create Terminal in Editor Side"
        },
        {
          "<leader><leader>'",
          function() call_vscode('workbench.action.createTerminalEditor') end,
          desc = "Create Terminal in Editor"
        },
        {
          "<leader>w",
          group = "Window",
        },
        {
          "<leader>wo",
          function() call_vscode('workbench.action.closeOtherEditors') end,
          desc = "Close Other Editors"
        },
        {
          "<leader>g",
          group = "Git",
        },
        {
          "<leader>gs",
          function() call_vscode('workbench.scm.focus') end,
          desc = "Git Status"
        },
        {
          "<leader>gl",
          function() call_vscode('workbench.scm.history.focus') end,
          desc = "Git Log"
        },
        {
          "<leader>gb",
          function() call_vscode('git.checkout') end,
          desc = "Git Checkout"
        },
        {
          "<leader>gf",
          function() call_vscode('git.pull') end,
          desc = "Git Pull"
        },
        {
          "<leader>gp",
          function() call_vscode('git.push') end,
          desc = "Git Push"
        },
        {
          "<leader>gz",
          group = "Git Stash",
        },
        {
          "<leader>gzz",
          function() call_vscode('git.stashIncludeUntracked') end,
          desc = "Git Stash All"
        },
        {
          "<leader>gzp",
          function() call_vscode('git.stashPopLatest') end,
          desc = "Git Stash Pop Latest"
        },
        {
          "<leader>m",
          group = "Clojure",
        },
        {
          "<leader>mc",
          function() call_vscode('calva.connectNonProjectREPL') end,
          desc = "Connect to a running REPL (non-project)"
        },
        {
          "<leader>mD",
          function() call_vscode('calva.disconnect') end,
          desc = "Disconnect from a running REPL"
        },
        {
          "<leader>mi",
          function() call_vscode('calva.jackIn') end,
          desc = "(Re)Start a project REPL and connect (Jack-In)"
        },
        {
          "<leader>mk",
          function() call_vscode('calva.jackOut') end,
          desc = "Stop/Kill the project REPL started by Calva (Jack-Out)"
        },
        {
          "<leader>mr",
          function() call_vscode('calva.showReplWindow') end,
          desc = "Show REPL window"
        },
        {
          "<leader>me",
          function() call_vscode('calva.loadFile') end,
          desc = "Evaluate entire file"
        },
        {
          "<leader>mg",
          function() call_vscode('calva.debug.instrument') end,
          desc = "Instrument current form for debugging"
        },
        {
          "<leader>mh",
          function() call_vscode('paredit.openList') end,
          desc = "Move cursor to list start"
        },
        {
          "<leader>ml",
          function() call_vscode('paredit.closeList') end,
          desc = "Move cursor to list end"
        },
        {
          "<leader>mb",
          function() call_vscode('paredit.barfSexpForward') end,
          desc = "Barf forwards"
        },
        {
          "<leader>mB",
          function() call_vscode('paredit.barfSexpBackward') end,
          desc = "Barf backwards"
        },
        {
          "<leader>ms",
          function() call_vscode('paredit.slurpSexpForward') end,
          desc = "Slurp forwards"
        },
        {
          "<leader>mS",
          function() call_vscode('paredit.slurpSexpBackward') end,
          desc = "Slurp backwards"
        },
        {
          "<leader>md",
          group = "Delete",
        },
        {
          "<leader>mdh",
          function() call_vscode('paredit.killListBackward') end,
          desc = "Delete to start of list"
        },
        {
          "<leader>mdl",
          function() call_vscode('paredit.killListForward') end,
          desc = "Delete to end of list"
        },
        {
          "<leader>mdu",
          function() call_vscode('paredit.raiseSexp') end,
          desc = "Delete parent and raise children"
        },
        {
          "<leader>mw",
          group = "Wrap",
        },
        {
          "<leader>mw(",
          function() call_vscode('paredit.wrapAroundParens') end,
          desc = "Wrap with ()"
        },
        {
          "<leader>mw[",
          function() call_vscode('paredit.wrapAroundSquare') end,
          desc = "Wrap with []"
        },
        {
          "<leader>mw{",
          function() call_vscode('paredit.wrapAroundCurly') end,
          desc = "Wrap with {}"
        },
        {
          "<leader>mw\"",
          function() call_vscode('paredit.wrapAroundQuote') end,
          desc = "Wrap with \"\""
        },
        {
          "<leader>mf",
          group = "Fiddle",
        },
        {
          "<leader>mff",
          function() call_vscode('calva.openFiddleForSourceFile') end,
          desc = "Open fiddle for source file"
        },
        {
          "<leader>mfF",
          function() call_vscode('calva.openSourceFileForFiddle') end,
          desc = "Open source file for fiddle"
        },
        {
          "<leader>mfe",
          function() call_vscode('calva.evaluateFiddleForSourceFile') end,
          desc = "Evaluate fiddle for source code"
        },
      }
    }
  },
  install = {},
  checker = { enabled = true },
})
