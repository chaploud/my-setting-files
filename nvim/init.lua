-- lazy.nvim bootstrap
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  print("Installing lazy.nvim...")
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

-- lazy.nvim setup
require("lazy").setup({
  -- ここにプラグインを追加していきます
  -- 例:
  -- 'nvim-lua/plenary.nvim', -- 多くのプラグインが依存する便利関数ライブラリ
  -- 'nvim-treesitter/nvim-treesitter', -- より高度なシンタックスハイライトなど

  -- カラースキームの例
  -- 'folke/tokyonight.nvim',

}, {
  -- lazy.nvim 自体の設定 (オプション)
  install = {
    -- missing = true, -- 起動時に足りないプラグインを自動インストール (デフォルトtrue)
    colorscheme_pre_setup = true, -- カラースキームを先に読み込む
  },
  checker = {
    enabled = true, -- 起動時にプラグインの更新をチェック
    notify = false, -- 更新があっても通知しない (お好みでtrueに)
  },
  change_detection = {
    enabled = true,
    notify = false, -- 設定変更時の自動再読み込み通知 (お好みでtrueに)
  },
})

print("init.lua loaded!") -- 読み込み確認用 (任意)
vim.opt.ignorecase = true
vim.opt.smartcase = true
