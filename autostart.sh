#!/usr/bin/env bash

# ----------------------------------------
# 🔁 Run helper (no duplicates)
# ----------------------------------------
run() {
  if ! pgrep -x "$(basename "$1")" > /dev/null; then
    "$@" &
  fi
}

# ----------------------------------------
# 🔑 Keyring (MUST BE FIRST)
# ----------------------------------------
KEYRING_DAEMON_PATH=$(nix eval --raw nixpkgs#gnome-keyring)/bin/gnome-keyring-daemon
eval "$(${KEYRING_DAEMON_PATH} --start --components=secrets,ssh,pkcs11)"

# ----------------------------------------
# 🧹 Cleanup
# ----------------------------------------
pkill conky 2>/dev/null

# ----------------------------------------
# ⚙️ Core services
# ----------------------------------------
run xfce4-power-manager
run copyq

# Polkit agent (Nix-safe path)
run $(nix eval --raw nixpkgs#polkit_gnome)/libexec/polkit-gnome-authentication-agent-1

# ----------------------------------------
# 🖱️ UI basics
# ----------------------------------------
xsetroot -cursor_name left_ptr &

run ~/.config/scripts/wallpaper.sh

# ----------------------------------------
# 🔔 Tray / background apps
# ----------------------------------------
run nm-tray
run volumeicon
run blueberry-tray
run flameshot
run dunst -conf ~/.config/dunst/dunstrc

# ----------------------------------------
# 🌡️ Redshift
# ----------------------------------------
run redshift -P -l 43.8:-79.3 -O 4000

# ----------------------------------------
# 📊 Conky (staggered startup)
# ----------------------------------------
conky -c ~/.config/xmonad/scripts/system-overview.conkyrc &

(
  sleep 2
  conky -c ~/.config/xmonad/scripts/AUR-Allinone.conkyrc &
) &

# ----------------------------------------
# 📊 Polybar (needs delay)
# ----------------------------------------
(
  sleep 2
  ~/.config/polybar/launch.sh
) &

# ----------------------------------------
# 🔊 Audio default
# ----------------------------------------
/run/current-system/sw/bin/amixer set Master 10% unmute &

# ----------------------------------------
# 🎨 PICOM (FIXED - START LAST)
# ----------------------------------------
(
  sleep 3

  # kill any broken early instance
  pkill picom 2>/dev/null

  # start compositor cleanly
  picom --experimental-backends --config ~/.config/picom/picom.conf
) &