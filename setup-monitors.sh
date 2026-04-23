#!/usr/bin/env bash
# XMonad Dual Monitor Setup Script
# HDMI-2 (left) + HDMI-1 (right) - Safe 1080p @ 60Hz

echo "=== Setting up monitors: HDMI-2 (left) + HDMI-1 (right) ==="

# Turn off HDMI-2 first to clear any bad mode
xrandr --output HDMI-2 --off
sleep 0.5

# Apply safe configuration
xrandr \
  --output HDMI-2 --mode 1920x1080 --rate 100 --primary --left-of HDMI-1 \
  --output HDMI-1 --mode 1920x1080 --rate 100

echo "Monitor setup completed."