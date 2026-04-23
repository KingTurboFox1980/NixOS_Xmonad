{ config, pkgs, ... }:

let
  hostname = "K10";

  upgradeScript = pkgs.writeScript "nixos-smart-upgrade" ''
    #!${pkgs.bash}/bin/bash
    set -euo pipefail

    FLAKE="/etc/nixos#${hostname}"
    LOG="/var/log/nixos-auto-upgrade.log"

    echo "==== $(date '+%Y-%m-%d %H:%M:%S') ====" >> "$LOG"
    cd /etc/nixos || { echo "cd failed" >> "$LOG"; exit 1; }

    echo "Updating flake inputs..." >> "$LOG"
    nix flake update >> "$LOG" 2>&1

    echo "Building new system..." >> "$LOG"
    if ! NEW=$(nix build "$FLAKE" --no-link --print-out-paths 2>>"$LOG"); then
      ${pkgs.dunst}/bin/dunstify -u critical "❌ NixOS Upgrade Failed" "Build failed — check $LOG"
      exit 1
    fi

    CURRENT=$(readlink -f /run/current-system)

    if [ "$NEW" = "$CURRENT" ]; then
      echo "No changes — skipping" >> "$LOG"
      ${pkgs.dunst}/bin/dunstify -u low "✔ NixOS Auto Upgrade" "No updates available"
      exit 0
    fi

    echo "Switching system..." >> "$LOG"
    if ! nixos-rebuild switch --flake "$FLAKE" -L >> "$LOG" 2>&1; then
      ${pkgs.dunst}/bin/dunstify -u critical "❌ NixOS Upgrade Failed" "Switch failed — check $LOG"
      exit 1
    fi

    # Summary
    DIFF_RAW=$(nix store diff-closures "$CURRENT" /run/current-system 2>/dev/null || echo "Diff unavailable")
    ADDED=$(echo "$DIFF_RAW" | grep -c "^+" || echo 0)
    REMOVED=$(echo "$DIFF_RAW" | grep -c "^-" || echo 0)
    UPDATED=$(echo "$DIFF_RAW" | grep -c "^->" || echo "$ADDED")

    SUMMARY="Upgraded: $UPDATED   Added: $ADDED   Removed: $REMOVED"
    echo "$SUMMARY" >> "$LOG"
    echo "$DIFF_RAW" >> "$LOG"

    ${pkgs.dunst}/bin/dunstify -u normal -t 25000 "🛠 NixOS Upgraded" "$SUMMARY"
  '';
in
{
  systemd.tmpfiles.rules = [
    "f /var/log/nixos-auto-upgrade.log 644 root root -"
  ];

  systemd.services.nixos-smart-upgrade = {
    description = "Smart NixOS Auto Upgrade";
    after = [ "network-online.target" ];
    wants = [ "network-online.target" ];

    serviceConfig = {
      Type = "oneshot";
      ExecStart = upgradeScript;   # This is the key change
      Nice = 19;
      IOSchedulingClass = "idle";
    };
  };

  systemd.timers.nixos-smart-upgrade = {
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnCalendar = "Sun 05:00";
      RandomizedDelaySec = "45min";
      Persistent = true;
    };
  };
}