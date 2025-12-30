{ config, pkgs, lib, ... }:

{
  # =========================
  # 🔧 CORE SYSTEM SERVICES
  # =========================

  services = {
    # 📦 App Management
    flatpak.enable = true;

    # 💾 Disk/Volume Management (Crucial for Thunar-volman)
    udisks2.enable = true;
    gvfs.enable = true;
    tumbler.enable = true;
    fstrim.enable = true;

    # 🔋 Power/Battery Management
    power-profiles-daemon.enable = true;
    upower.enable = true;

    # 🎧 Audio Configuration (PipeWire)
    pipewire = {
      enable = true;
      audio.enable = true;
      pulse.enable = true;
      alsa.enable = true; 
      alsa.support32Bit = true;
      jack.enable = true;
      wireplumber.enable = true;
    };

    # 🔑 Secrets Management
    gnome.gnome-keyring.enable = true;

    # 🖨️ Printing
    printing.enable = true;

    # ⏱️ Time Synchronization
    timesyncd.enable = true;
  };

  # 🖥️ XDG Portals (MANDATORY FOR 25.05 / UNSTABLE)
  # This section prevents the massive 'webkitgtk' builds by explicitly
  # defining which portal backend to use.
  xdg.portal = {
    enable = true;
    extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
    config = {
      common = {
        default = [ "gtk" ];
      };
    };
  };

  # 👮 System Policy Management
  security.polkit.enable = true;

  # 🖥️ Virtualization (QEMU/KVM)
  programs.virt-manager.enable = true;
  virtualisation = {
    libvirtd.enable = true;
    spiceUSBRedirection.enable = true;
  };

  # 🔥 Firewall
  networking.firewall.enable = true;


  # =========================
  # 📦 GLOBAL DESKTOP PACKAGES
  # =========================

  environment.systemPackages = with pkgs; [
    # 🔑 Secrets and Keyring Utilities
    libsecret
    gnome-keyring
    seahorse

    # 📁 Thunar and associated utilities (As requested)
    xfce.thunar
    xfce.thunar-archive-plugin
    xfce.thunar-volman # Essential for your removable media mounting

    # 🌐 Network Share Support
    gvfs
    samba
  ];
}