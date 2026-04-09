# ~/.config/xmobar/generate-xmobar.sh
#!/usr/bin/env bash

source ~/.cache/wal/colors.sh

bg="$background"
fg="$foreground"
accent="$color1"

template_file="$HOME/.config/xmobar/xmobar.template.hs"
output_file="$HOME/.xmonad/xmobar.hs"

# Replace placeholders
sed \
  -e "s|\$wal_bg|$bg|g" \
  -e "s|\$wal_fg|$fg|g" \
  -e "s|\$wal_template|<fc=$accent>%datetime%</fc> { <fc=$fg>%memory%</fc> <fc=$fg>%cpu%</fc> }|g" \
  "$template_file" > "$output_file"
