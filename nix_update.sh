#!/usr/bin/env bash

# NixOS Maintenance & Update Menu
flake_path="/etc/nixos"
script_path="$0"

clear

# Configure Cachix cache if not already done.
echo "Checking and configuring Cachix cache..."
if command -v cachix &>/dev/null; then
    # Use sudo as 'cachix use' modifies system-wide Nix configuration
    sudo cachix use crazazy &>/dev/null
    if [ $? -eq 0 ]; then
        echo "✅ Cachix cache 'crazazy' configured/active for this session."
    else
        echo "⚠️ Failed to activate 'crazazy' cachix cache. Might need password or specific permissions."
    fi
else
    echo "⚠️ 'cachix' command not found. Install it (e.g., 'nix-env -iA cachix -f https://cachix.org/api/v1/install') for faster builds."
fi
echo # Add a newline for spacing

echo "🩺 NixOS Maintenance Menu"
echo "Select an action:"
echo
echo " 1) 📦  Update Flatpak Apps"
echo " 2) 👤  Update User Environment Packages (nix-env)"
echo " 3) ⬆️  Update Flake Inputs & Rebuild System (Full Upgrade)"
echo " 4) ⬆️  Update Software Channel & Switch (Legacy/Non-Flake)"
echo " 5) 🧪  Test NixOS Flakes Rebuild"
echo " 6) ℹ️  List NixOS Generations"
echo " 7) 🧹  System Cleanup & Optimization"
echo " 8) 🚀  Standard NixOS Flakes Rebuild (No Input Update)"
echo " 9) 🗑️  Delete a Specific Generation"
echo "10) 🗑️  Delete All Old Generations"
echo "11) 🧹  Clean Up Boot Menu"
echo "12) 🔄  Update Flake Inputs Only (No Rebuild)"
echo "13) 📅  View Auto-Upgrade Timer Status"
echo "14) 🩺  Check Auto-Upgrade Service Health"
echo "15) 📝  Edit This Menu Script"
echo "16) 🚪  Exit"
echo
read -rp "Enter your choice: " choice

case $choice in
 1)
    # Original: 11) Update Flatpak Apps
    echo "📦 Updating Flatpak apps..."
    flatpak update
    echo "✅ Flatpaks updated!"
    ;;

 2)
    # Original: 12) Update User Environment Packages (nix-env)
    echo "👤 Updating user-installed packages (nix-env)..."
    nix-env -u '*'
    echo "✅ User packages updated!"
    ;;

 3)
    # Original: 2) Update Flake Inputs & Rebuild System (Full Upgrade)
    echo "⬆️ Updating flake inputs and rebuilding system..."
    cd "$flake_path" || exit
    sudo nix flake update
    sudo nixos-rebuild switch --flake .
    ;;

 4)
    # Original: 9) Update Software Channel & Switch (Legacy/Non-Flake)
    echo "⬆️ Updating channel and switching system (non-flake)..."
    nix-channel --update nixos
    cd "$flake_path" && sudo nixos-rebuild switch --upgrade
    ;;

 5)
    # Original: 4) Test NixOS Flakes Rebuild
    echo "🧪 Performing test rebuild..."
    cd "$flake_path" || exit
    if sudo nixos-rebuild build --flake .; then
      echo "✅ Build passed."
      read -rp "Proceed with switch? (Y/n): " proceed
      if [[ "${proceed^^}" == "Y" || -z "$proceed" ]]; then
        sudo nixos-rebuild switch --flake .
      else
        echo "⏩ Skipping switch."
      fi
    else
      echo "❌ Build failed. Please check errors."
    fi
    ;;

 6)
    # Original: 5) List NixOS Generations
    echo "ℹ️ Listing generations..."
    sudo nix-env -p /nix/var/nix/profiles/system --list-generations
    ;;

 7)
    # Original: 1) System Cleanup & Optimization
    echo "🧹 Running cleanup..."
    sudo nix-collect-garbage
    sudo nix store optimise
    sudo nix-collect-garbage -d
    nix profile wipe-history --older-than 100d
    echo "✅ Cleanup complete!"
    ;;

 8)
    # Original: 3) Standard NixOS Flakes Rebuild (Without Updating Inputs)
    echo "🚀 Rebuilding NixOS with flakes (no input update)..."
    cd "$flake_path" || exit
    sudo nixos-rebuild switch --flake .
    ;;

 9)
    # Original: 6) Delete a Specific Generation
    read -rp "Enter generation to delete: " gen
    echo "🗑️ Deleting generation $gen..."
    sudo nix-env -p /nix/var/nix/profiles/system --delete-generations "$gen"
    cd "$flake_path" && sudo nixos-rebuild boot
    ;;

 10)
    # Original: 7) Delete All Old Generations
    echo "🗑️ Deleting all old generations..."
    sudo nix-env -p /nix/var/nix/profiles/system --delete-generations old
    cd "$flake_path" && sudo nixos-rebuild boot
    ;;

 11)
    # Original: 8) Clean Up Boot Menu
    echo "🧹 Rebuilding boot menu..."
    cd "$flake_path" && sudo nixos-rebuild boot
    ;;

 12)
    # Original: 10) Update Flake Inputs Only (No Rebuild)
    echo "🔄 Updating flake inputs only..."
    cd "$flake_path" && sudo nix flake update
    echo "✅ Flake inputs updated. Run option 8 to apply."
    ;;

 13)
    # Original: 13) View Auto-Upgrade Timer Status
    echo "📅 Auto-upgrade timer status:"
    systemctl status nixos-upgrade.timer
    ;;

 14)
    # Original: 14) Check Auto-Upgrade Service Health
    echo "🩺 Auto-upgrade service health:"
    systemctl status nixos-upgrade.service
    ;;

 15)
    # Original: 15) Edit This Menu Script
    echo "📝 Opening this script..."
    kitty sudo nvim "$script_path"
    ;;

 16)
    # Original: 16) Exit
    echo "🚪 Goodbye!"
    exit 0
    ;;

 *)
    echo "⚠️ Invalid selection."
    ;;
esac

echo
read -rp "Press Enter to exit."
