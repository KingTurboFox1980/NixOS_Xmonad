{ config, pkgs, ... }:

{
  # Enable dconf for proper graphical integration with virt-manager
  programs.dconf.enable = true;

  # Install necessary packages.
  environment.systemPackages = with pkgs; [
    # Core GUI and interaction tools
    virt-manager
    virt-viewer
    spice-gtk
    adwaita-icon-theme

    # Core QEMU, KVM, and Networking dependencies
    qemu               # This provides all necessary qemu system binaries
    dnsmasq            # For libvirtd's default NAT network (DHCP/DNS)
    bridge-utils       # For advanced network setups
    iptables           # For network rule management
  ];

  # Manage the core virtualisation services.
  virtualisation = {
    libvirtd = {
      enable = true;
      # Configure libvirtd to automatically manage networks and VMs
      onBoot = "start";
      onShutdown = "shutdown";

      qemu = {
        # Enable Secure Boot (OVMF) and TPM support.
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

        # KVM is enabled implicitly when libvirtd is enabled.
      };
    };
    # Enable Spice for clipboard sharing and graphical quality enhancements
    spiceUSBRedirection.enable = true;
  };

  # Spice daemon for guest interaction
  services.spice-vdagentd.enable = true;
}
