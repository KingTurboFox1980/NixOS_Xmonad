{
  description = "K10 NixOS (picom-jonaburg, polybar fixed, qtwebengine allowed)";

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

    overlays = [
      # ─────────────────────────────────────────────
      # Picom (jonaburg)
      # ─────────────────────────────────────────────
      (final: prev: {
        picom-jonaburg = prev.stdenv.mkDerivation {
          pname = "picom-jonaburg";
          version = "git";
          src = picom-jonaburg-src;

          nativeBuildInputs = with prev; [
            meson ninja pkg-config asciidoc
          ];

          buildInputs = with prev; [
            libGL
            libx11
            libxext
            libconfig
            libxdg_basedir
            libev
            glib
            dbus
            pcre
            libdrm
            cairo
            pango
            freetype
            libxml2
            uthash
            libxcb-render-util
            libxcb-image
          ];

          mesonBuildType = "release";
        };

        picom = final.picom-jonaburg;
      })

      # ─────────────────────────────────────────────
      # Polybar: force older toolchain (stable fix)
      # ─────────────────────────────────────────────
      (final: prev: {
        polybar = prev.polybar.override {
          stdenv = prev.gcc13Stdenv;
        };
      })
    ];

    pkgs = import nixpkgs {
      inherit system overlays;

      config = {
        allowUnfree = true;

        permittedInsecurePackages = [
          "qtwebengine-5.15.19"
        ];
      };
    };
  in {
    nixosConfigurations.K10 = nixpkgs.lib.nixosSystem {
      inherit system;
      pkgs = pkgs;

      modules = [
        ./configuration.nix
      ];
    };
  };
}
