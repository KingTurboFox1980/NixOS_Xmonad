{ config, pkgs, ... }:

{
  # Enable dconf for proper graphical integration with virt-manager
  programs.dconf.enable = true;

  # Install necessary packages
  environment.systemPackages = with pkgs; [
    # Core GUI and interaction tools
    virt-manager
    virt-viewer
    spice-gtk
    adwaita-icon-theme

    # Thunar and plugins (per your requirements)
    xfce.thunar
    xfce.thunar-archive-plugin
    xfce.thunar-volman

    # Core QEMU, KVM, and Networking dependencies
    qemu
    dnsmasq
    bridge-utils
    iptables
  ];

  # Manage the core virtualisation services
  virtualisation.libvirtd = {
    enable = true;
    onBoot = "start";
    onShutdown = "shutdown";
  };
}