{ config, pkgs, ... }:

{
  # Enable dconf
  programs.dconf.enable = true;

  # Install necessary packages.
  environment.systemPackages = with pkgs; [
    virt-manager
    virt-viewer
    spice-gtk
    adwaita-icon-theme 
  ];

  # Manage the virtualisation services.
  virtualisation = {
    libvirtd = {
      enable = true;
      qemu = {
        # Enable Secure Boot and TPM support.
        ovmf = {
          enable = true;
          packages = [
            (pkgs.OVMFFull.override {
              secureBoot = true;
              tpmSupport = true;
            })
          ];
        };
        swtpm.enable = true;
      };
    };
    spiceUSBRedirection.enable = true;
  };
  services.spice-vdagentd.enable = true;
}
