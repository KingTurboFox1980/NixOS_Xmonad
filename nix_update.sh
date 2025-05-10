#!/usr/bin/env bash

# Flake path (corrected to /etc/nixos)
flake_path="/etc/nixos"
# Get the current script path
script_path="$0"

echo "Select an option:"
echo "1) 🛠️ Recompile XMonad"
echo "2) 🔄 Restart XMonad"
echo "3) 🚀 Standard NixOS Flakes Rebuild"
echo "4) 🧪 Test NixOS Flakes Rebuild"
echo "5) ℹ️ List NixOS Generations"
echo "6) 🗑️ Delete a Specific NixOS Generation"
echo "7) 🗑️ Delete All Old Generations"
echo "8) 🧹 Clean Up Boot Menu"
echo "9) ⬆️ Update Software Channel"
echo "10) 🧹 System Cleanup & Optimization"
echo "11) 🔍 Monitor Recent Updates"
echo "12) 🚀 Manual Flake Update and Rebuild"
echo "13) 📝 Edit This Menu"
echo "14) 🚪 Exit"

read -p "Enter your choice: " choice

case $choice in
  1)
    echo "🛠️ Recompiling XMonad..."
    xmonad --recompile
    ;;
  2)
    echo "🔄 Restarting XMonad..."
    xmonad --restart
    ;;
  3)
    echo "🚀 Running standard NixOS flakes rebuild..."
    cd "$flake_path" || exit
    sudo nixos-rebuild switch --flake .
    ;;
  4)
    echo "🧪 Testing NixOS flakes rebuild (dry-run)..."
    cd "$flake_path" || exit
    if sudo nixos-rebuild build --flake .; then
      echo "✅ Test build completed successfully."
      read -p "Do you want to proceed with the full NixOS flakes rebuild? (Y/n): " proceed
      proceed=${proceed:-Y}
      if [[ "$proceed" =~ ^[Yy]$ ]]; then
        sudo nixos-rebuild switch --flake .
      else
        echo "⏩ Skipping full rebuild."
      fi
    else
      echo "❌ Test build encountered errors. Please check the output."
    fi
    ;;
  5)
    echo "ℹ️ Listing NixOS Generations..."
    sudo nix-env -p /nix/var/nix/profiles/system --list-generations
    ;;
  6)
    read -p "Enter the generation number to delete: " generation_to_delete
    echo "🗑️ Deleting NixOS Generation $generation_to_delete..."
    sudo nix-env -p /nix/var/nix/profiles/system --delete-generations "$generation_to_delete"
    echo "🧹 Cleaning up the Boot Menu..."
    cd "$flake_path" || exit
    sudo nixos-rebuild boot
    ;;
  7)
    echo "🗑️ Deleting all old NixOS Generations..."
    sudo nix-env -p /nix/var/nix/profiles/system --delete-generations old
    echo "🧹 Cleaning up the Boot Menu..."
    cd "$flake_path" || exit
    sudo nixos-rebuild boot
    ;;
  8)
    echo "🧹 Cleaning up the Boot Menu..."
    cd "$flake_path" || exit
    sudo nixos-rebuild boot
    ;;
  9)
    echo "⬆️ Updating Software Channel..."
    nix-channel --update nixos
    cd "$flake_path" || exit
    sudo nixos-rebuild switch --upgrade
    ;;
  10)
    echo "🧹 Running System Cleanup & Optimization..."
    echo "🗑️ Cleaning up the Store..."
    sudo nix-collect-garbage
    echo "⚙️ Optimizing Store..."
    sudo nix store optimise
    echo "💽 Reclaiming Disk Space..."
    sudo nix-collect-garbage -d
    echo "🧹 Clearing Cache..."
    nix profile wipe-history --older-than 100d
    echo "✅ System cleanup completed!"
    ;;
  11)
    echo "🔍 Monitoring Recent Updates..."
    journalctl -u system-autoUpgrade.service --no-pager | tail -n 50
    ;;
  12)
    echo "🚀 Running manual Flake update and rebuild..."
    cd "$flake_path" || exit
    sudo nix flake update
    sudo nixos-rebuild switch --flake .
    ;;
  13)
    echo "📝 Editing this menu..."
    kitty sudo vim "$script_path"
    ;;
  14)
    echo "🚪 Exiting..."
    exit 0
    ;;
  *)
    echo "⚠️ Invalid selection. Please choose a valid option."
    ;;
esac

echo "Press Enter to exit."
read -r
