#!/bin/bash

# ノートPCのタッチパッドを有効化/無効化します
# 有効化[y]
# 無効化[n]

echo "無効化[y] / 有効化[n]"
read -r -p "選択してください: " REPLY

TOUCHPAD_ID=$(xinput | grep 'PNP0C50:00 06CB:CDAA Touchpad' | grep -o 'id=[0-9]*' | sed 's/id=//')

if [ "$REPLY" = "y" ]; then
  xinput disable "$TOUCHPAD_ID"
elif [ "$REPLY" = "n" ]; then
  xinput enable "$TOUCHPAD_ID"
else
  echo "何もしません"
  exit 1
fi

if [ "$REPLY" = "y" ]; then
  echo "無効化しました"
elif [ "$REPLY" = "n" ]; then
  echo "有効化しました"
fi
