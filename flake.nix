{
  description = "NixOS flake for K10 - Fixed Read-Only conflict";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    picom-jonaburg-src = {
      url = "github:jonaburg/picom";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, picom-jonaburg-src, ... }:
    let
      system = "x86_64-linux";
      
      # Define our custom pkgs with overlays
      myPkgs = import nixpkgs {
        inherit system;
        config = {
          allowUnfree = true;
          # FIX: Add the insecure package here
          permittedInsecurePackages = [
            "qtwebengine-5.15.19"
          ];
        };
        overlays = [
          (final: prev: {
            picom-jonaburg = prev.stdenv.mkDerivation {
              pname = "picom-jonaburg";
              version = "custom-v7"; 
              src = picom-jonaburg-src;
              nativeBuildInputs = with prev; [ meson ninja pkg-config asciidoc ];
              buildInputs = with prev; [
                libGL xorg.libXext xorg.libX11 libconfig libxdg_basedir libev glib
                dbus pcre libdrm cairo pango freetype libxml2
                xorg.xcbutilrenderutil xorg.xcbutilimage uthash
              ];
              mesonBuildType = "release";
            };
            picom = final.picom-jonaburg;
          })
        ];
      };
    in {
      nixosConfigurations.K10 = nixpkgs.lib.nixosSystem {
        specialArgs = { inherit self; }; 

        modules = [
          {
            nixpkgs.pkgs = myPkgs;
          }

          ./configuration.nix

          ({ pkgs, ... }: {
            environment.systemPackages = with pkgs; [
              xfce.thunar
              xfce.thunar-archive-plugin
              xfce.thunar-volman
            ];
          })
        ];
      };
    };
}
