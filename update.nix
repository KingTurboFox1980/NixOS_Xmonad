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
    dates = "Sat *-*-* 05:00:00";
    randomizedDelaySec = "45min";
    persistent = true;
  };

  # Add a post-upgrade notification using systemd override
  systemd.services.nixos-upgrade.serviceConfig.ExecStartPost = [
    "${pkgs.dunst}/bin/dunstify -u normal -t 10000 '🛠 NixOS Auto Upgrade' 'System has been successfully updated.'"
  ];
}
