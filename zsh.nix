{ pkgs, lib, config, ... }:

{
  ##########################################################################
  # Zsh (minimal, safe)
  ##########################################################################
  programs.zsh = {
    enable = true;

    shellAliases = {
      # Standard utilities
      ll   = "ls -lh";
      home = "cd ~";

      # Color aliases
      ls   = "ls --color=auto";
      grep = "grep --color=auto";

      # Flake and system management
      update = "sudo nixos-rebuild switch";
      fu     = "(cd /etc/nixos && sudo nixos-rebuild switch --flake .#K10)";
      fub    = "(cd /etc/nixos && nix build .#nixosConfigurations.K10.config.system.build.toplevel)";
      up     = "sudo /home/j3ll0/nix_update.sh";

      # Maintenance
      trim = "sudo fstrim -v /";

      # Navigation and editor
      dot = "cd /etc/nixos/";
      v   = "sudo -E nvim";

      # XMonad
      keys = ".config/scripts/update_xmonad_keys.sh";
    };

    # Starship must be initialized in interactive shells
    interactiveShellInit = lib.optionalString config.programs.starship.enable ''
      eval "$(starship init zsh)"
    '';
  };

  ##########################################################################
  # Starship
  ##########################################################################
  programs.starship.enable = true;
}
