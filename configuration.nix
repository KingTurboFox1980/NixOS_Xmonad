{ config, lib, pkgs, ... }:   

let   
  vivaldi = pkgs.vivaldi.overrideAttrs (oldAttrs: {   
    dontWrapQtApps = false;   
    dontPatchELF = true;   
    nativeBuildInputs = oldAttrs.nativeBuildInputs ++ [pkgs.kdePackages.wrapQtAppsHook];   
  });   
in   

{  
  imports = [  
    ./hardware-configuration.nix  
  ];  

  # Bootloader Configuration  
  boot.loader.systemd-boot.enable = true;  
  boot.loader.efi.canTouchEfiVariables = true;  

  # Networking  
  networking.hostName = "nixos";  
  networking.networkmanager.enable = true;  

  # Time and Locale Settings  
  time.timeZone = "America/Toronto";  
  i18n.defaultLocale = "en_CA.UTF-8";  

  # Enable X server with XMonad 
  services.xserver.enable = true; 
  services.xserver.xkb.layout = "us";  
  services.xserver.windowManager.xmonad.enable = true;

  # Set Cursor Theme for X Server
  environment.variables = {
    XCURSOR_THEME = "Material-Black";
    XCURSOR_SIZE = "24";
    GTK_THEME = "Dracula";
    GTK_ICON_THEME = "Papirus-Dark";
  };

  # Enable GNOME Keyring & Polkit
  services.gnome.gnome-keyring.enable = true;  
  security.polkit.enable = true;  

  # User Account Configuration  
  users.users.j3ll0 = {  
    isNormalUser = true;  
    description = "Angelo";  
    extraGroups = [ "networkmanager" "wheel" "libvirtd" ];  
    packages = with pkgs; [ kdePackages.kate ];  
  };  

  # Enable virtualization  
  virtualisation.libvirtd.enable = true;  
  
  # Asus 
  services.asusd.enable = true;

  # USB Mount
  services.udisks2.enable = true;
  
  # GVFS
  services.gvfs.enable = true;

  # Performance Optimizations  
  zramSwap.enable = true;  
  programs.xfconf.enable = true;  
  boot.kernel.sysctl."vm.dirty_ratio" = 20;  

  # Mount Additional Storage  
  fileSystems."/mnt/downloads" = {  
    device = "/dev/disk/by-uuid/4681ad39-ed76-4fe7-ab87-d3a03816a8a1";  
    fsType = "ext4";  
    options = [ "defaults" ];  
  };  

  fileSystems."/tmp" = {  
    device = "tmpfs";  
    fsType = "tmpfs";  
    options = [ "size=2G" "mode=1777" ];  
  };  

  boot.kernelParams = [ "mitigations=off" ];  
 
  # Scheduled System Upgrades  
  system.autoUpgrade = {  
    enable = true;  
    flags = [ "--upgrade" "-L" ];  
    dates = "Sun *-*-* 05:00:00";  
    randomizedDelaySec = "30min";  
    persistent = true;  
  };  

  nix.settings.cores = 12;  
  nix.gc.automatic = true;  
  nix.gc.dates = "Sun *-*-* 05:00:00";  

  # Auto-Rebuild System After Updates  
  systemd = {  
    services.autoRebuild = {  
      enable = true;  
      description = "Auto-rebuild system after updates";  
      serviceConfig = {  
        ExecStart = "${pkgs.bash}/bin/bash -c 'nix-channel --update && nixos-rebuild switch'";  
      };  
    };  
    timers.autoRebuild = {  
      enable = true;  
      description = "Trigger auto-rebuild system";  
      timerConfig = {  
        OnCalendar = "Sun *-*-* 05:00:00";  
        Persistent = true;  
      };  
      wantedBy = [ "timers.target" ];  
    };  
  };  

  # Enable Unfree Packages  
  nix.settings.experimental-features = [ "nix-command" "flakes" ];  
  nixpkgs.config.allowUnfree = true;  

  # Merge All Packages into One `environment.systemPackages`
  environment.systemPackages = with pkgs; [ 
    material-cursors

    # XMonad & Dependencies  
    xmonad-with-packages 
    haskellPackages.xmonad-contrib 
    dmenu 
    rofi 
    xdotool
    pkgs.haskellPackages.xmonad-contrib
    pkgs.trayer
    pkgs.xfce.xfce4-screenshooter
    pkgs.xfce.xfce4-terminal
    pkgs.dunst
    pkgs.gtk3
    python3
    python3Packages.requests
    python3Packages.configparser

    # Browsers & Networking  
    firefox 
    microsoft-edge 
    p3x-onenote 
    vivaldi 
    whatsapp-for-linux  
    pkgs.networkmanager
    pkgs.nm-tray

    # Development Tools  
    evolutionWithPlugins 
    geany 
    git 
    neovim 
    vim 
    vscode  

    # File Management  
    pkgs.cifs-utils
    samba
    gvfs
    pkgs.cifs-utils
    kdePackages.dolphin 
    rclone 
    rclone-browser 
    xfce.thunar 
    xfce.thunar-archive-plugin 
    xfce.thunar-volman  

    # Multimedia & Theming  
    alsa-utils 
    brightnessctl 
    pkgs.pywal 
    vlc  
    pkgs.xwallpaper
    pkgs.volumeicon
    pkgs.blueberry
    
    # UI Customization  
    arandr 
    arc-theme 
    catppuccin-gtk
    dracula-theme
    gnome-tweaks 
    material-cursors 
    pkgs.picom  
    pkgs.redshift
    
    # Bars  
    haskellPackages.xmobar 
    pkgs.polybar  

    # System Tools & Virtualization  
    pkgs.preload 
    sxhkd 
    pkgs.asusctl
    bitwarden-desktop 
    bleachbit 
    gnome-extension-manager 
    gnomeExtensions.open-bar 
    gnomeExtensions.tray-icons-reloaded 
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
    pkgs.xfce.xfce4-power-manager
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
    pkgs.copyq
    qbittorrent 
    vdhcoapp 
    wget 
    yt-dlp  
  ];  

  # System State Version  
  system.stateVersion = "24.11";  
}
