{ config, pkgs, lib, ... }:

{
  # ==============================
  # 🧑‍💻 USER AND SHELL CONFIGURATION
  # ==============================

  # Set Zsh as the default shell
  users.users.j3ll0.shell = pkgs.zsh;

  # Enable Zsh and Starship prompt
  programs.zsh.enable = true;
  programs.starship.enable = true;

  # 🪐 Inject Starship into Zsh sessions using lib.mkIf for clarity.
  # FIX: We now correctly access the option via 'config.programs.zsh.enable'
  environment.shellInit = lib.mkIf config.programs.zsh.enable ''
    eval "$(starship init zsh)"
  '';


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
