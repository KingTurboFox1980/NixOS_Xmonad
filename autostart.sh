#!/usr/bin/env bash

# Modular run function to avoid duplicate processes
function run {
  if ! pgrep -x "$1" > /dev/null; then
    "$@" &
  fi
}

# 🔑 CRITICAL FIX: Keyring Environment Setup (MUST RUN FIRST)
# This uses 'nix eval' to find the gnome-keyring-daemon and then uses 'eval'
# to execute the shell commands it generates. This correctly sets environment 
# variables (like GNOME_KEYRING_PID and SSH_AUTH_SOCK) in the current shell, 
# allowing Evolution and other applications to find the running keyring service.
KEYRING_DAEMON_PATH=$(nix eval --raw nixpkgs#gnome-keyring)/bin/gnome-keyring-daemon
eval "$(${KEYRING_DAEMON_PATH} --start --components=secrets,ssh,pkcs11)"
# -----------------------------------------------------


# --- Cleanup ---
# Ensure a clean slate for Conky, which is prone to issues on startup
killall conky

# --- Core services and managers (that rely on D-Bus) ---

# xfce4-power-manager ensures battery management is active
if ! pgrep -x xfce4-power-manager > /dev/null; then
    xfce4-power-manager &
fi

run copyq

# 🧠 Polkit agent (fixed path via nix eval)
# This agent handles the graphical prompt for unlocking the Keyring
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
sleep 2

# 3. Launch the second Conky (Key Hints)
conky -c ~/.config/xmonad/scripts/AUR-Allinone.conkyrc &

# -----------------------------------------------------
# 🚀 Polybar (Often needs a delay as well)
# -----------------------------------------------------
(sleep 2; run ~/.config/polybar/launch.sh) &

# 🧩 Optional apps (uncomment as needed)
# ...
