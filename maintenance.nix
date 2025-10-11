{ config, pkgs, ... }:

{
  # Enable automatic garbage collection.
  # This option is a top-level option under `nix`, not `services.nix`.
  nix.gc = {
    automatic = true;
    dates = "weekly"; # Runs once a week
    options = "--delete-older-than 7d"; # Deletes generations older than 7 days
  };

  # Automatically run `nix-store --optimise` to deduplicate files.
  nix.settings.auto-optimise-store = true;
}
