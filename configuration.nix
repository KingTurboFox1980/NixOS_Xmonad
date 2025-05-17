# May 17, 2025 # 

# https://github.com/KingTurboFox1980/xmonad.git # 

{ config, lib, pkgs, ... }:

let
  # Improved Vivaldi override for Qt compatibility
  vivaldi = pkgs.vivaldi.overrideAttrs (oldAttrs: {
    dontWrapQtApps = false;
    dontPatchELF = true;
    nativeBuildInputs = oldAttrs.nativeBuildInputs ++ [ pkgs.kdePackages.wrapQtAppsHook ];
  });

in
{
  # 🖥️ Hardware Configuration
  imports = [ ./hardware-configuration.nix ];

  # 🚀 Bootloader Settings
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # 🌐 Networking Configuration
  networking = {
    hostName = "nixos";
    networkmanager.enable = true;
  };

  # ⏳ Time & Locale Settings
  time.timeZone = "America/Toronto";
  i18n.defaultLocale = "en_CA.UTF-8";

  # 🖥️ X Server & XMonad Window Manager
  services.xserver = {
    enable = true;
    xkb.layout = "us";
    windowManager.xmonad.enable = true;
  };

  # 🎨 UI Customization
  environment.variables = {
    XCURSOR_THEME = "Material-Black";
    XCURSOR_SIZE = "24";
    GTK_THEME = "Dracula";
    GTK_ICON_THEME = "Papirus-Dark";
  };

  # 🔐 Security & Authentication
  services.gnome.gnome-keyring.enable = true;
  security.polkit.enable = true;

  # 👤 User Configuration
  users.users.j3ll0 = {
    isNormalUser = true;
    description = "Angelo";
    extraGroups = [ "networkmanager" "wheel" "libvirtd" ];
    packages = with pkgs; [ kdePackages.kate ];
  };

  # 🏗️ Virtualization Support
  programs.virt-manager.enable = true;
  virtualisation = {
    libvirtd.enable = true;
    spiceUSBRedirection.enable = true;
  };

  # ⚙️ System Services
  services = {
    asusd.enable = true;  # ASUS utilities
    udisks2.enable = true; # USB mounting
    gvfs.enable = true;   # Virtual filesystem
  };

  # 🔥 Preload Service for Faster App Launching
  systemd.services.preload = {
    enable = true;
    description = "Preload Adaptive Prefetching Daemon";
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      ExecStart = "/run/current-system/sw/bin/preload";
      Restart = "always";
    };
  };

  # 🚀 Performance Optimization
  zramSwap.enable = true;
  programs.xfconf.enable = true;
  boot.kernel.sysctl."vm.dirty_ratio" = 20;

  # 🗄️ Storage Configuration
  fileSystems = {
    "/mnt/downloads" = {
      device = "/dev/disk/by-uuid/4681ad39-ed76-4fe7-ab87-d3a03816a8a1";
      fsType = "ext4";
      options = [ "defaults" ];
    };
    "/tmp" = {
      device = "tmpfs";
      fsType = "tmpfs";
      options = [ "size=2G" "mode=1777" ];
    };
  };

  # ⚙️ Kernel Parameters
  boot.kernelParams = [ "mitigations=off" ];

  # 🔄 Auto System Updates
  system.autoUpgrade = {
    enable = true;
    flags = [ "--upgrade" "-L" ];
    dates = "Sun *-*-* 05:00:00";
    randomizedDelaySec = "30min";
    persistent = true;
  };

  # 🛠️ Enable Experimental Features
  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  nixpkgs.config.allowUnfree = true;

  # 🎭 Font Configuration
  fonts = {
    enableFontDir = true;
    enableGhostscriptFonts = true;
    packages = with pkgs; [
      corefonts vistafonts inconsolata terminus_font proggyfonts dejavu_fonts
      font-awesome ubuntu_font_family source-code-pro source-sans-pro source-serif-pro
      noto-fonts-emoji openmoji-color twemoji-color-font udev-gothic-nf texlivePackages.inconsolata-nerd-font
    ];
  };

  # 🏗️ Installed System Packages
  environment.systemPackages = with pkgs; [

    # XMonad & Dependencies
    xmonad-with-packages 
    haskellPackages.xmonad-contrib 
    dmenu 
    rofi 
    xdotool 
    trayer 
    xfce.xfce4-screenshooter 
    xfce.xfce4-terminal
    dunst 
    gtk3 
    xorg.xprop 
    jq 
    python3 
    python3Packages.requests 
    python3Packages.configparser

    # Browsers & Networking
    firefox 
    microsoft-edge 
    p3x-onenote 
    vivaldi 
    whatsapp-for-linux 
    networkmanager 
    nm-tray

    # Development Tools
    evolutionWithPlugins 
    geany 
    git 
    neovim 
    vim 
    vscode

    # File Management
    samba 
    gvfs 
    kdePackages.dolphin 
    rclone 
    rclone-browser 
    xfce.thunar 
    xfce.thunar-archive-plugin 
    xfce.thunar-volman

    # Multimedia & Theming
    alsa-utils 
    brightnessctl 
    pywal 
    vlc 
    xwallpaper 
    volumeicon 
    blueberry 
    nitrogen

    # UI Customization
    arandr 
    arc-theme 
    catppuccin-gtk 
    dracula-theme 
    gnome-tweaks 
    material-cursors 
    picom 
    redshift 
    lxappearance-gtk2

    # Bars
    haskellPackages.xmobar 
    polybar

    # System Tools & Virtualization
    preload 
    pkgs.mission-center
    sxhkd 
    asusctl 
    bitwarden-desktop 
    bleachbit 
    conky 
    lm_sensors 
    gnome-calendar 
    gnome-disk-utility 
    mate.engrampa
    networkmanager 
    networkmanagerapplet 
    polkit_gnome 
    pulseaudioFull 
    swtpm 
    sysstat 
    virt-manager 
    virtiofsd 
    xclip 
    xfce.xfce4-power-manager 
    xorg.xev

    # Terminal Utilities
    alacritty 
    btop 
    fastfetch 
    fzf 
    kitty 
    starship 
    zsh

    # Other Utilities
    copyq 
    qbittorrent 
    vdhcoapp 
    wget 
    yt-dlp
  ];

  # 🏗️ System State Version
  system.stateVersion = "24.11";
}
