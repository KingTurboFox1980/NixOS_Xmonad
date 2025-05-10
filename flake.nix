{ 
  description = "NixOS Configuration with XMonad and Essential Packages"; 

  inputs = { 
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable"; 
  }; 

  outputs = { self, nixpkgs, ... }: 
  let 
    system = "x86_64-linux"; # Adjust if your system architecture is different 
    pkgs = nixpkgs.legacyPackages.${system}; 
  in 
  { 
    nixosConfigurations.nixos = nixpkgs.lib.nixosSystem { 
      inherit system; 
      modules = [ 
        ./configuration.nix 
        ./hardware-configuration.nix 
      ]; 
    }; 
  }; 
} 
