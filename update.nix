{ config, pkgs, ... }:

{
  system.autoUpgrade = {
    enable = true;
    flake = "/etc/nixos#K10";
    flags = [
      "--update-input" "nixpkgs"
      "--commit-lock-file"
      "-L"
    ];

    # Every 2 weeks (1st and 15th), Sunday at 05:00
    dates = "Sun *-*-01,15 05:00:00";

    randomizedDelaySec = "45min";
    persistent = true;
  };

  # Post-upgrade notification
  systemd.services.nixos-upgrade.serviceConfig.ExecStartPost = [
    "${pkgs.dunst}/bin/dunstify -u normal -t 10000 '🛠 NixOS Auto Upgrade' 'System has been successfully updated.'"
  ];
}
