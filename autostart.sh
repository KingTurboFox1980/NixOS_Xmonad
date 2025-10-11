#!/usr/bin/env bash

# Modular run function to avoid duplicate processes
function run {
  if ! pgrep -x "$1" > /dev/null; then
    "$@" &
  fi
}

# --- Cleanup ---
# Ensure a clean slate for Conky, which is prone to issues on startup
killall conky

# --- Core services and managers (that rely on D-Bus) ---

# xfce4-power-manager is listed twice, keeping one check and one run for clarity
if ! pgrep -x xfce4-power-manager > /dev/null; then
    xfce4-power-manager &
fi

run copyq
run sxhkd -c $HOME/.config/sxhkd/sxhkdrc # Assuming sxhkd is intended to run

# 🧠 Polkit agent (fixed path via nix eval)
run $(nix eval --raw nixpkgs#polkit_gnome)/libexec/polkit-gnome-authentication-agent-1

# 🖱️ Cursor
xsetroot -cursor_name left_ptr &

# 🎨 Wallpaper
run ~/.config/scripts/wallpaper.sh

# 📊 System tray and indicators
run nm-tray
run volumeicon
run blueberry-tray
run org.flameshot.Flameshot
run dunst -conf ~/.config/dunst/dunstrc 

# 🖥️ COMPOSITOR & TEMPERATURE
run picom --experimental-backends --config ~/.config/picom/picom.conf
run redshift -P -l 43.8:-79.3 -O 4000

# -----------------------------------------------------
# 📊 CONKY LAUNCH WITH DELAYS
# -----------------------------------------------------

# 1. Launch the first Conky immediately (System Overview)
conky -c ~/.config/xmonad/scripts/system-overview.conkyrc &

# 2. Wait a few seconds for the X server/WM to settle and the first Conky to draw.
sleep 5

# 3. Launch the second Conky (Key Hints)
# NOTE: Using AUR-Allinone.conkyrc based on your script. Ensure this is the correct path!
conky -c ~/.config/xmonad/scripts/AUR-Allinone.conkyrc &

# -----------------------------------------------------
# 🚀 Polybar (Often needs a delay as well)
# -----------------------------------------------------
(sleep 2; run ~/.config/polybar/launch.sh) &

# 🧩 Optional apps (uncomment as needed)
# ...
