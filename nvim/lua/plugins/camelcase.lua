return {
  -- プラグインのGitHubリポジトリ名を指定
  'bkad/CamelCaseMotion',

  -- プラグインをロードするタイミングを指定
  -- "BufReadPre": ファイルを読み込む直前 (すぐに使いたい場合におすすめ)
  -- "VeryLazy": NeoVim起動後、少しアイドル時間があった後 (起動速度重視の場合)
  -- どちらでも大きな問題はありませんが、ここでは BufReadPre にしておきます。
  event = "BufReadPre",

  -- プラグインがロードされた後に実行する設定
  config = function()
    -- キーマッピングを簡単に設定するためのショートカット
    local map = vim.keymap.set
    -- which-key.nvim などで説明を表示するための共通オプション
    local opts = { desc = "CamelCase Motion" }

    -- ここで <Plug>CamelCaseMotion_* という特殊なマッピングを
    -- 実際に使うキー (<leader>w など) に割り当てます。
    -- <leader> は通常 '\' (バックスラッシュ) キーです。

    -- ノーマルモード (n) でのキーマッピング
    map('n', '<leader>w', '<Plug>CamelCaseMotion_w', { desc = opts.desc .. " forward" })
    map('n', '<leader>b', '<Plug>CamelCaseMotion_b', { desc = opts.desc .. " backward" })
    map('n', '<leader>e', '<Plug>CamelCaseMotion_e', { desc = opts.desc .. " end" })
    map('n', '<leader>ge', '<Plug>CamelCaseMotion_ge', { desc = opts.desc .. " end backward" })
    -- inner/around word オブジェクトもマッピングしておくと便利 (例: di<leader>w で削除)
    map('n', '<leader>iw', '<Plug>CamelCaseMotion_iw', { desc = opts.desc .. " inner word" })
    map('n', '<leader>aw', '<Plug>CamelCaseMotion_aw', { desc = opts.desc .. " a word" })

    -- ビジュアルモード (x) でのキーマッピング (選択範囲で移動)
    map('x', '<leader>w', '<Plug>CamelCaseMotion_w', { desc = opts.desc .. " forward" })
    map('x', '<leader>b', '<Plug>CamelCaseMotion_b', { desc = opts.desc .. " backward" })
    map('x', '<leader>e', '<Plug>CamelCaseMotion_e', { desc = opts.desc .. " end" })
    map('x', '<leader>ge', '<Plug>CamelCaseMotion_ge', { desc = opts.desc .. " end backward" })
    map('x', '<leader>iw', '<Plug>CamelCaseMotion_iw', { desc = opts.desc .. " inner word" })
    map('x', '<leader>aw', '<Plug>CamelCaseMotion_aw', { desc = opts.desc .. " a word" })

    -- オペレーターペンディングモード (o) でのキーマッピング (d<leader>w などで使う)
    map('o', '<leader>w', '<Plug>CamelCaseMotion_w', { desc = opts.desc .. " forward" })
    map('o', '<leader>b', '<Plug>CamelCaseMotion_b', { desc = opts.desc .. " backward" })
    map('o', '<leader>e', '<Plug>CamelCaseMotion_e', { desc = opts.desc .. " end" })
    map('o', '<leader>ge', '<Plug>CamelCaseMotion_ge', { desc = opts.desc .. " end backward" })
    map('o', '<leader>iw', '<Plug>CamelCaseMotion_iw', { desc = opts.desc .. " inner word" })
    map('o', '<leader>aw', '<Plug>CamelCaseMotion_aw', { desc = opts.desc .. " a word" })
  end,
}

