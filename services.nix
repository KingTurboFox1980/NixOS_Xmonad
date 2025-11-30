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

    # 🔑 Secrets Management (Essential for saving network share passwords, etc.)
    gnome.gnome-keyring.enable = true;

    # 🖨️ Printing
    printing.enable = true;
  };

  # 🖥️ XDG Portals (Ensures correct file dialogs/screensharing in Flatpak apps)
  xdg.portal = {
    enable = true;
    extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
  };

  # 👮 System Policy Management
  security.polkit.enable = true;

  # 🖥️ Virtualization (QEMU/KVM)
  programs.virt-manager.enable = true;
  virtualisation = {
    libvirtd.enable = true;
    spiceUSBRedirection.enable = true;
  };


  # =========================
  # 🛡️ SECURITY AND SYSTEM UTILITIES
  # =========================

  # ⏱️ Time Synchronization
  services.timesyncd.enable = true;

  # 🔥 Firewall
  networking.firewall.enable = true;


  # =========================
  # 📦 GLOBAL DESKTOP PACKAGES
  # =========================

  environment.systemPackages = with pkgs; [
    # 🔑 Secrets and Keyring Utilities
    libsecret
    gnome-keyring
    seahorse # GUI for managing keys and passwords

    # 📁 Thunar and associated utilities
    xfce.thunar
    xfce.thunar-archive-plugin
    xfce.thunar-volman # Handles automatic mounting of removable media

    # 🌐 GVFS Backend for Network Shares
    gvfs
    samba # Enables access to Samba/Windows shares (smb:// protocol)
  ];
}
