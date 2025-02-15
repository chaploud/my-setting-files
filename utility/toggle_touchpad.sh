#!/bin/bash

# ノートPCのタッチパッドを有効化/無効化します
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

SETTING_1=("KERNEL==\"event*\"" "ATTRS{name}==\"PS/2 Synaptics TouchPad\"" "ENV{LIBINPUT_IGNORE_DEVICE}=\"${NUM}\"")
SETTING_1_STRING=$(IFS=,; echo "${SETTING_1[*]}")
SETTING_2=("KERNEL==\"event*\"" "ATTRS{name}==\"PNP0C50:00 06CB:CDAA Touchpad\"" "ENV{LIBINPUT_IGNORE_DEVICE}=\"${NUM}\"")
SETTING_2_STRING=$(IFS=,; echo "${SETTING_2[*]}")
SETTING_3=("KERNEL==\"event*\"" "ATTRS{name}==\"PNP0C50:00 06CB:CDAA Mouse\"" "ENV{LIBINPUT_IGNORE_DEVICE}=\"${NUM}\"")
SETTING_3_STRING=$(IFS=,; echo "${SETTING_3[*]}")

sudo tee /etc/udev/rules.d/99-ignore-touchpad.rules <<EOF
$SETTING_1_STRING
$SETTING_2_STRING
$SETTING_3_STRING
EOF
sudo udevadm control --reload-rules
sudo udevadm trigger

if [ "$REPLY" = "y" ]; then
  echo "無効化しました"
elif [ "$REPLY" = "n" ]; then
  echo "有効化しました"
fi
