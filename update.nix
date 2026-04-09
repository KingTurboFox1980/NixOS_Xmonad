{ config, pkgs, ... }:

let
  upgradeScript = pkgs.writeShellScript "nixos-smart-upgrade" ''
    set -euo pipefail

    FLAKE="/etc/nixos#K10"
    LOG="/var/log/nixos-auto-upgrade.log"

    echo "==== $(date) ====" >> $LOG

    # Step 1: Update flake inputs
    nix flake update /etc/nixos >> $LOG 2>&1

    # Step 2: Build new system (no switch yet)
    nix build "$FLAKE" --no-link --print-out-paths > /tmp/new-system 2>>$LOG

    NEW=$(cat /tmp/new-system)
    CURRENT=$(readlink /run/current-system)

    if [ "$NEW" = "$CURRENT" ]; then
      echo "No changes detected. Skipping upgrade." >> $LOG

      ${pkgs.dunst}/bin/dunstify -u low -t 5000 \
        "✔ NixOS Auto Upgrade" \
        "No updates available."

      exit 0
    fi

    echo "Changes detected. Upgrading..." >> $LOG

    # Step 3: Perform upgrade
    nixos-rebuild switch --flake "$FLAKE" -L >> $LOG 2>&1

    # Step 4: Compute diff
    DIFF_RAW=$(${pkgs.nix}/bin/nix store diff-closures \
      "$CURRENT" \
      /nix/var/nix/profiles/system 2>/dev/null || true)

    # Step 5: Group counts
    ADDED=$(echo "$DIFF_RAW" | grep "^+" | wc -l)
    REMOVED=$(echo "$DIFF_RAW" | grep "^-" | wc -l)

    # Upgraded = pairs of + and - with same pkg (rough estimate)
    UPDATED=$(echo "$DIFF_RAW" | grep "^→" | wc -l || true)

    # Fallback if no → symbols (older nix)
    if [ "$UPDATED" -eq 0 ]; then
      UPDATED=$(echo "$DIFF_RAW" | grep "^+" | wc -l)
    fi

    # Step 6: Short preview (first 15 lines)
    PREVIEW=$(echo "$DIFF_RAW" | head -n 15)

    if [ -z "$PREVIEW" ]; then
      PREVIEW="(No detailed diff available)"
    fi

    SUMMARY="↑ $UPDATED  + $ADDED  - $REMOVED"

    echo "$SUMMARY" >> $LOG
    echo "$DIFF_RAW" >> $LOG

    # Step 7: Notify
    ${pkgs.dunst}/bin/dunstify -u normal -t 20000 \
      "🛠 NixOS Upgraded" \
      "$SUMMARY\n\n$PREVIEW"
  '';
in
{
  systemd.services.nixos-smart-upgrade = {
    description = "Smart NixOS Auto Upgrade";
    serviceConfig = {
      Type = "oneshot";
      ExecStart = upgradeScript;
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