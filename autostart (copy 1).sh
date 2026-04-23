#!/usr/bin/env bash
# ================================================
# XMonad Autostart - Xmobar (Top) + Polybar (Bottom)
# Fixed timing for reliable boot
# ================================================

echo "=== XMonad Autostart Started at $(date) ==="


# Give XMonad time to fully initialize
sleep 4

# ================================================
# 🖥️ MONITOR SETUP
# ================================================
echo "Setting up monitors..."
~/.config/xmonad/scripts/setup-monitors.sh
sleep 2.5

# ================================================
# 🔑 GNOME Keyring (Best version for NixOS)
# ================================================
echo "Starting GNOME Keyring..."

# Kill any existing instances first
pkill -x gnome-keyring-daemon 2>/dev/null || true
sleep 0.5

# Start the daemon
eval "$(/run/current-system/sw/bin/gnome-keyring-daemon --start --components=secrets,ssh,pkcs11 2>/dev/null)"

# Try to automatically unlock the "Login" keyring
dbus-send --session --dest=org.gnome.keyring --type=method_call \
          /org/gnome/keyring/daemon org.gnome.keyring.Daemon.Unlock \
          string:"Login" 2>/dev/null || true

echo "GNOME Keyring started and unlock attempted."

# Helper
run() {
    if ! pgrep -x "$(basename "$1")" >/dev/null 2>&1; then
        "$@" &
    fi
}

# ================================================
# Core Services
# ================================================
run xfce4-power-manager
run copyq
run "$(nix eval --raw nixpkgs#polkit_gnome)/libexec/polkit-gnome-authentication-agent-1"

# Basics
xsetroot -cursor_name left_ptr &
run ~/.config/scripts/wallpaper.sh

run nm-tray
run volumeicon
run blueberry-tray
run flameshot
run dunst -conf ~/.config/dunst/dunstrc
run redshift -P -l 43.8:-79.3 -O 4000

# Conky
conky -c ~/.config/xmonad/scripts/system-overview.conkyrc &
(sleep 3 && conky -c ~/.config/xmonad/scripts/AUR-Allinone.conkyrc &) &

(
    sleep 3
    echo "Launching Polybar (bottom)..."
    ~/.config/polybar/launch.sh &
) &

# ================================================
# Audio + Picom (last)
# ================================================
/run/current-system/sw/bin/amixer set Master 10% unmute &

(
    sleep 8
    pkill -x picom 2>/dev/null || true
    echo "Starting Picom..."
    picom --experimental-backends --config ~/.config/picom/picom.conf &
) &

echo "=== XMonad Autostart Completed ==="