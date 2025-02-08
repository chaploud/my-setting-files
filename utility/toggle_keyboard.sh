#!/bin/bash

# ノートPCのキーボードを有効化/無効化します
# 有効化[y]
# 無効化[n]

echo "無効化[y] / 有効化[n]"
read -r -p "選択してください: " REPLY
if [ "$REPLY" = "y" ]; then
  SETTING='KERNEL=="event*", ATTRS{name}=="AT Translated Set 2 keyboard", ENV{LIBINPUT_IGNORE_DEVICE}="1"'
elif [ "$REPLY" = "n" ]; then
  SETTING='KERNEL=="event*", ATTRS{name}=="AT Translated Set 2 keyboard", ENV{LIBINPUT_IGNORE_DEVICE}="0"'
else
  echo "何もしません"
  exit 1
fi

sudo tee /etc/udev/rules.d/99-ignore-keyboard.rules <<< "$SETTING"
sudo udevadm control --reload-rules
sudo udevadm trigger

if [ "$REPLY" = "y" ]; then
  echo "無効化しました"
elif [ "$REPLY" = "n" ]; then
  echo "有効化しました"
fi