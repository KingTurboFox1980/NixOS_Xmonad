#!/usr/bin/env bash

# Check if picom is running
if pgrep -x "picom" > /dev/null
then
    # If it is, kill it and send a notification
    pkill picom
    dunstify -h string:x-dunst-stack-tag:picom "Picom **OFF**" -i "dialog-information"
else
    # If it's not, start it and send a notification
    picom -b --config ~/.config/picom/picom.conf
    dunstify -h string:x-dunst-stack-tag:picom "Picom **ON**" -i "dialog-information"
fi
