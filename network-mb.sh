#!/usr/bin/env bash

# You will need to replace 'enp0s31f6' with your actual network interface name.
# You can find this by running 'ip a' in your terminal.
INTERFACE="enp4s0"

RX_OLD=$(cat /sys/class/net/$INTERFACE/statistics/rx_bytes)
TX_OLD=$(cat /sys/class/net/$INTERFACE/statistics/tx_bytes)

# Wait for a second to calculate the rate
sleep 1

RX_NEW=$(cat /sys/class/net/$INTERFACE/statistics/rx_bytes)
TX_NEW=$(cat /sys/class/net/$INTERFACE/statistics/tx_bytes)

# Calculate the difference and convert to MB/s
# The corrected line uses the full path to 'bc'
RX_RATE=$(echo "scale=1; ($RX_NEW - $RX_OLD) / 1024 / 1024" | /usr/bin/bc)
TX_RATE=$(echo "scale=1; ($TX_NEW - $TX_OLD) / 1024 / 1024" | /usr/bin/bc)

# Print the formatted output
printf "🔺%.1fMB/s|🔻%.1fMB/s" "$TX_RATE" "$RX_RATE"
