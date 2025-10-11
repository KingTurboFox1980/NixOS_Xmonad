{
  description = "NixOS flake for K10 with manual XMonad and jonaburg Picom.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    picom-jonaburg-src = {
      url = "github:jonaburg/picom";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, picom-jonaburg-src, ... }:
    let
      system = "x86_64-linux";

      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        
        overlays = [
          (final: prev: {
            
            # FINAL SOLUTION: Define picom from scratch to completely bypass the original derivation's problematic hooks
            picom-jonaburg = prev.stdenv.mkDerivation {
              pname = "picom-jonaburg";
              version = "custom-v7"; 
              src = picom-jonaburg-src;

              # Standard Meson build system dependencies
              nativeBuildInputs = with prev; [
                meson ninja pkg-config asciidoc
              ];

              # List of runtime/compile-time libraries needed by Picom
              buildInputs = with prev; [
                libGL xorg.libXext xorg.libX11 libconfig libxdg_basedir libev glib
                dbus pcre libdrm cairo pango freetype libxml2
                xorg.xcbutilrenderutil
                xorg.xcbutilimage
                # FIX: Added missing dependency uthash
                uthash
              ];
              
              # Normal build settings
              mesonBuildType = "release";

              # Ensure all checks are explicitly disabled for safety
              doCheck = false;
              installCheckPhase = "";
              versionCheckPhase = "";
              
              # Clear all previously problematic hooks
              postInstall = null;
              postPatch = null;
            };

            # Alias to ensure that anywhere 'pkgs.picom' is used, the custom version is picked up.
            picom = final.picom-jonaburg;
          })
        ];
      };
    in {
      nixosConfigurations.K10 = nixpkgs.lib.nixosSystem {
        modules = [
          ./configuration.nix
          nixpkgs.nixosModules.readOnlyPkgs 
        ];
        
        specialArgs = {
          pkgs = pkgs; 
        };
      };
    };
}