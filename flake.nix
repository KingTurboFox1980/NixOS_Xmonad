{
  description = "NixOS Configuration with XMonad and Extended Packages";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    xmonad.url = "github:xmonad/xmonad"; # Main XMonad package
    xmonad-contrib.url = "github:xmonad/xmonad-contrib"; # XMonad contrib modules
  };

  outputs = { self, nixpkgs, xmonad, xmonad-contrib, ... }:
  let
    system = "x86_64-linux"; # Adjust for different architectures
    pkgs = import nixpkgs { inherit system; };
  in {
    nixosConfigurations.nixos = nixpkgs.lib.nixosSystem {
      inherit system;
      modules = [
        ./configuration.nix
        ./hardware-configuration.nix
      ];
    };

    # Add an overlay to include XMonad packages
    nixpkgs.overlays = [
      (final: prev: {
        xmonad = xmonad.packages.${system}.xmonad;
        xmonad-contrib = xmonad-contrib.packages.${system}.xmonad-contrib;
      })
    ];
  };
}
