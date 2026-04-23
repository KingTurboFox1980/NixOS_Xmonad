#!/usr/bin/env bash

if pgrep -x "conky" > /dev/null; then
    pkill -x conky
else
    # 1. Get the hardware preference
    PREF=$(powerprofilesctl get 2>/dev/null || cat /sys/devices/system/cpu/cpufreq/policy0/energy_performance_preference)

    case "$PREF" in
        *performance*|*balance_performance*)
            export ACCENT="#FF79C6"      # Neon Pink for the Mode text
            export PCORE_COLOR="#FF5555" # Performance Red for P-Cores
            export ECORE_COLOR="#FFB86C" # Orange for E-Cores
            ;;
        *power-saver*|*energy_savings*)
            export ACCENT="#50FA7B"      # Neon Green for the Mode text
            export PCORE_COLOR="#50FA7B" # Green for P-Cores
            export ECORE_COLOR="#8BE9FD" # Cyan for E-Cores
            ;;
        *)
            # Default Dracula vibes
            export ACCENT="#FF79C6"
            export PCORE_COLOR="#BD93F9" # Purple
            export ECORE_COLOR="#8BE9FD" # Cyan
            ;;
    esac

    # 2. Launch
    conky -c "$HOME/.config/xmonad/scripts/AUR-Allinone.conkyrc" &
    conky -c "$HOME/.config/xmonad/scripts/system-overview.conkyrc" &
fi
