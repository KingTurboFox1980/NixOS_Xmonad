{ config, lib, pkgs, ... }:    

let    
  vivaldi = pkgs.vivaldi.overrideAttrs (oldAttrs: {    
    dontWrapQtApps = false;    
    dontPatchELF = true;    
    nativeBuildInputs = oldAttrs.nativeBuildInputs ++ [pkgs.kdePackages.wrapQtAppsHook];    
  });    
in    

{   
  # Import Hardware Configuration  
  imports = [ ./hardware-configuration.nix ];   

  # Bootloader Configuration  
  boot.loader = {  
    systemd-boot.enable = true;   
    efi.canTouchEfiVariables = true;   
  };  

  # Networking Configuration  
  networking = {  
    hostName = "nixos";   
    networkmanager.enable = true;   
  };  

  # Time & Locale Settings  
  time.timeZone = "America/Toronto";   
  i18n.defaultLocale = "en_CA.UTF-8";   

  # X Server & Window Manager  
  services.xserver = {  
    enable = true;  
    xkb.layout = "us";   
    windowManager.xmonad.enable = true;   
  };   

  # Set Cursor & Theme Preferences  
  environment.variables = {  
    XCURSOR_THEME = "Material-Black";  
    XCURSOR_SIZE = "24";  
    GTK_THEME = "Dracula";  
    GTK_ICON_THEME = "Papirus-Dark";  
  };  

  # Security & Authentication  
  services.gnome.gnome-keyring.enable = true;  
  security.polkit.enable = true;  

  # User Configuration  
  users.users.j3ll0 = {  
    isNormalUser = true;  
    description = "Angelo";  
    extraGroups = [ "networkmanager" "wheel" "libvirtd" ];  
    packages = with pkgs; [ kdePackages.kate ];  
  };   

  # Virtualization Support  
  programs.virt-manager.enable = true;  
  users.groups.libvirtd.members = [ "j3ll0" ];  
  virtualisation = {  
    libvirtd.enable = true;  
    spiceUSBRedirection.enable = true;   
  };   

  # System & Hardware Services  
  services = {  
    asusd.enable = true;      # ASUS-specific utilities  
    udisks2.enable = true;    # USB mounting support  
    gvfs.enable = true;       # GVFS for file system management  
  };  

  # Performance Optimization  
  zramSwap.enable = true;  
  programs.xfconf.enable = true;  
  boot.kernel.sysctl."vm.dirty_ratio" = 20;  

  # Storage Configuration  
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

  # Kernel Parameters  
  boot.kernelParams = [ "mitigations=off" ];  

  # System Update & Maintenance  
  system.autoUpgrade = {  
    enable = true;  
    flags = [ "--upgrade" "-L" ];  
    dates = "Sun *-*-* 05:00:00";  
    randomizedDelaySec = "30min";  
    persistent = true;  
  };  

  nix = {  
    settings.cores = 12;  
    gc = {  
      automatic = true;  
      dates = "Sun *-*-* 05:00:00";  
    };  
  };  

  # Auto-Rebuild System After Updates  
  systemd.services.autoRebuild = {  
    enable = true;  
    description = "Auto-rebuild system after updates";  
    serviceConfig.ExecStart = "${pkgs.bash}/bin/bash -c 'nix-channel --update && nixos-rebuild switch'";  
  };  
  systemd.timers.autoRebuild = {  
    enable = true;  
    timerConfig.OnCalendar = "Sun *-*-* 05:00:00";  
    timerConfig.Persistent = true;  
  };  

  # Enable Unfree Packages  
  nix.settings.experimental-features = [ "nix-command" "flakes" ];  
  nixpkgs.config.allowUnfree = true;  

  # Font Configuration  
  fonts = {  
    enableFontDir = true;  
    enableGhostscriptFonts = true;  
    packages = with pkgs; [  
      corefonts vistafonts inconsolata terminus_font  
      proggyfonts dejavu_fonts font-awesome ubuntu_font_family  
      source-code-pro source-sans-pro source-serif-pro  
      noto-fonts-emoji openmoji-color twemoji-color-font pkgs.udev-gothic-nf 
      pkgs.texlivePackages.inconsolata-nerd-font
    ];  
  };  

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
    pkgs.xorg.xprop
    pkgs.jq
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
    pkgs.pywal  
    vlc   
    pkgs.xwallpaper 
    pkgs.volumeicon 
    pkgs.blueberry 
    pkgs.nitrogen
 
    # UI Customization   
    arandr  
    arc-theme  
    catppuccin-gtk 
    dracula-theme 
    gnome-tweaks  
    material-cursors  
    pkgs.picom   
    pkgs.redshift 
    pkgs.lxappearance-gtk2
    
    # Bars   
    haskellPackages.xmobar  
    pkgs.polybar   

    # System Tools & Virtualization   
    pkgs.preload  
    sxhkd  
    pkgs.asusctl 
    bitwarden-desktop  
    bleachbit  
    pkgs.conky
    pkgs.lm_sensors
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
    pkgs.mate.engrampa
    qbittorrent  
    vdhcoapp  
    wget  
    yt-dlp   
  ];   

  # System State Version   
  system.stateVersion = "24.11";   
}
