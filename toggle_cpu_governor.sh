#!/usr/bin/env bash

# Script: /home/j3ll0/.config/xmobar/toggle_cpu_governor.sh
# Description: Cycles through CPU governors: performance -> powersave -> schedutil (or ondemand)

# === START LOGGING ===
# Redirect all stdout and stderr to a log file
LOG_FILE="/tmp/cpu_governor_toggle.log"
exec &> "$LOG_FILE"
echo "--- $(date) ---"
echo "Script started by Xmobar action."
# === END LOGGING ===

# Define the order of governors to cycle through
# IMPORTANT: Adjust this list based on what governors are available on your system.
# Check with: cpupower frequency-info
GOVERNORS=("performance" "powersave" "schedutil")
# Fallback if schedutil isn't available or preferred: GOVERNORS=("performance" "powersave" "ondemand")

# Path to the governor file for CPU0. We assume all CPUs use the same governor.
GOVERNOR_FILE="/sys/devices/system/cpu/cpu0/cpufreq/scaling_governor"

# Use absolute paths for commands
CPUPOWER_CMD="/usr/bin/cpupower" # <--- Full path
SUDO_CMD="/usr/bin/sudo"         # <--- Full path
AWK_CMD="/usr/bin/awk"           # <--- Full path
CAT_CMD="/usr/bin/cat"           # <--- Full path
TEE_CMD="/usr/bin/tee"           # <--- Full path

# Check if cpupower is available (it should be now, but let's be sure)
if "$CPUPOWER_CMD" frequency-info &> /dev/null; then
    USE_CPUPOWER=true
else
    USE_CPUPOWER=false
    echo "Error: '$CPUPOWER_CMD' not found. Cannot proceed without it."
    exit 1 # Exit immediately if cpupower isn't available
fi

# Get the current governor
if $USE_CPUPOWER; then
    CURRENT_GOVERNOR=$("$CPUPOWER_CMD" frequency-info -g | "$AWK_CMD" 'NR==2 {print $2}')
else
    # This block should ideally not be reached if cpupower is installed
    if [[ -f "$GOVERNOR_FILE" ]]; then
        CURRENT_GOVERNOR=$("$CAT_CMD" "$GOVERNOR_FILE")
    else
        echo "Error: Cannot determine current governor. '$GOVERNOR_FILE' not found."
        exit 1
    fi
fi

# Find the index of the current governor in our list
CURRENT_INDEX=-1
for i in "${!GOVERNORS[@]}"; do
    if [[ "${GOVERNORS[$i]}" == "$CURRENT_GOVERNOR" ]]; then
        CURRENT_INDEX=$i
        break
    fi
done

# Handle case where current governor is not in our defined list
if [[ $CURRENT_INDEX -eq -1 ]]; then
    echo "Warning: Current governor '$CURRENT_GOVERNOR' not in defined cycle. Starting from first governor in list."
    CURRENT_INDEX=${#GOVERNORS[@]} # Set to last index to cycle to first
fi

# Calculate the next governor's index
NEXT_INDEX=$(( (CURRENT_INDEX + 1) % ${#GOVERNORS[@]} ))
NEXT_GOVERNOR="${GOVERNORS[$NEXT_INDEX]}"

# Set the new governor
echo "Current governor: $CURRENT_GOVERNOR"
echo "Setting governor to: $NEXT_GOVERNOR"

if $USE_CPUPOWER; then
    # Use sudo with cpupower for all CPUs
    # You MUST have configured sudoers for passwordless execution of this command!
    "$SUDO_CMD" "$CPUPOWER_CMD" frequency-set -g "$NEXT_GOVERNOR"
    if [[ $? -ne 0 ]]; then
        echo "Error: Failed to set governor using '$CPUPOWER_CMD'. Check sudoers configuration or governor name."
        exit 1
    fi
fi

echo "CPU governor successfully changed to $NEXT_GOVERNOR."
echo "--- Script finished ---"
