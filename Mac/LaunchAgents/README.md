# LaunchAgents設定

macOSのLaunchAgentsを使用したサービスの自動起動設定ファイル群です。

## yaskkserv2 (SKKサーバー)

### 概要
yaskkserv2をシステム起動時に自動起動し、ターミナルを開かなくてもSKK日本語入力を使用できるようにします。

### 設定ファイル
- `com.yaskkserv2.plist` - yaskkserv2のLaunchAgent設定

### インストール方法

1. 設定ファイルをLaunchAgentsディレクトリにコピー

```sh
cp com.yaskkserv2.plist ~/Library/LaunchAgents/
```

2. LaunchAgentをロードして起動

```sh
launchctl load ~/Library/LaunchAgents/com.yaskkserv2.plist
```

3. 起動確認

```sh
lsof -i :1178
```

### 管理コマンド

```sh
# サービスの停止
launchctl unload ~/Library/LaunchAgents/com.yaskkserv2.plist

# サービスの開始
launchctl load ~/Library/LaunchAgents/com.yaskkserv2.plist

# サービスの状態確認
launchctl list | grep yaskkserv2

# ログの確認
tail -f /tmp/yaskkserv2.out
tail -f /tmp/yaskkserv2.err
```

### 設定内容

- ポート: 1178
- 辞書ファイル: ~/Install/dictionary.yaskkserv2
- キャッシュファイル: /tmp/yaskkserv2.cache
- ログファイル: yaskkserv2.out, yaskkserv2.err
- 自動起動: ログイン時
- プロセス監視: 異常終了時に自動再起動

### メリット

1. ログイン時に自動起動: ターミナルを開かなくてもSKKサーバーが利用可能
1. プロセス監視: 異常終了時に自動再起動
1. システム統合: macOSの標準的なサービス管理方法
1. ログ管理: 標準出力・エラー出力をファイルに記録

### 注意事項

- 辞書ファイルのパス（~/Install/dictionary.yaskkserv2）は環境に合わせて調整してください
- すでに他の方法でyaskkserv2を起動している場合は、重複起動を避けるため停止してください

## 導入履歴

2025-07-30: yaskkserv2のLaunchAgent設定を追加
ポート1178でyaskkserv2を自動起動
ログイン時の自動起動とプロセス監視機能
