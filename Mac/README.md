# Macの開発環境整備

最終更新日: 2025-11-06

- Raycast
- Ghostty
- Emacs

## CLI

- git (2.50.1)
- fzf (0.66.1)
- rg (15.1.0)
- volta (2.0.2)
  - node (v24.9.0)
  - npm (11.6.0)

## 環境再チェック

### TODO

- [ ] starshipからシンプルなものに乗り換え
- [ ] Ghosttyの導入と基本キーバインド把握
- [ ] emacsclientによる高速起動は興味あり
  - [ ] ただ、プロセスのkillやバッファ全部削除はよくやりたくなりそう
  - [ ] 調査

### `~/.zshrc`

#### Git系エイリアス

- ほぼ全ての基本操作は省略形で使えるから積極的に使おう

### `starship`からシンプルに

- 必要な要素

```toml
[git_status]
format = '$all_status $ahead_behind '
diverged = '[￪${ahead_count}](cyan)/[￬${behind_count}](red)'
conflicted = '[=${count}](red)'
ahead = '[￪${count}](cyan)'
behind = '[￬${count}](red)'
untracked = '[?${count}](cyan)'
stashed = '[#${count}](blue)'
modified = '[!${count}](yellow)'
staged = '[+${count}](green)'
renamed = '[r${count}](blue)'
deleted = '[x${count}](red)'
[git_branch]
format = '[$branch(:$remote_branch)](bold purple) '
[directory]
format = '[$path](bold blue)[$read_only]($read_only_style) '
truncate_to_repo = false
truncation_symbol = "../"
[username]
show_always = true
format = '[$user](bold green) '
```
