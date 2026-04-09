#!/usr/bin/env bash

# If ANY conky is running, kill them all
if pgrep -x "conky" > /dev/null; then
    pkill -x conky
else
    # Launch both configs simultaneously
    conky -c "$HOME/.config/xmonad/scripts/AUR-Allinone.conkyrc" &
    conky -c "$HOME/.config/xmonad/scripts/system-overview.conkyrc" &
fi
