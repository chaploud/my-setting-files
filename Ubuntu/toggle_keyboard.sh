#!/bin/bash

# ノートPCのキーボードを有効化/無効化します
# 有効化[y]
# 無効化[n]

echo "無効化[y] / 有効化[n]"
read -r -p "選択してください: " REPLY
if [ "$REPLY" = "y" ]; then
  NUM=1
elif [ "$REPLY" = "n" ]; then
  NUM=0
else
  echo "何もしません"
  exit 1
fi

SETTING=("KERNEL==\"event*\"" "ATTRS{name}==\"AT Translated Set 2 keyboard\"" "ENV{LIBINPUT_IGNORE_DEVICE}=\"${NUM}\"")
SETTING_STRING=$(IFS=,; echo "${SETTING[*]}")

sudo tee /etc/udev/rules.d/99-ignore-keyboard.rules <<< "$SETTING_STRING"
sudo udevadm control --reload-rules
sudo udevadm trigger

if [ "$REPLY" = "y" ]; then
  echo "無効化しました"
elif [ "$REPLY" = "n" ]; then
  echo "有効化しました"
fi
