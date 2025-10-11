{ pkgs, ... }:

{
  # System-wide packages to install.
  environment.systemPackages = with pkgs; [
    # General Applications
    p3x-onenote
    whatsapp-for-linux
    
    # Browsers
    firefox
    vivaldi # vivaldi is now in the official nixpkgs, so override isn't needed

    # Networking
    networkmanager
    nm-tray # Re-added, if you still want a tray applet for NetworkManager, though `networkmanagerapplet` is also available.

    # Development Tools
    cachix
    evolution
    geany
    git
    neovim
    (python3.withPackages (ps: [ ps.psutil ]))
    vscode

    # File Management
    samba
    rclone
    rclone-browser

    # XFCE Thunar Components
    xfce.thunar
    xfce.thunar-archive-plugin
    xfce.thunar-volman
    samba
    gvfs
    cifs-utils

    # Multimedia & Viewers
    alsa-utils
    brightnessctl
    pulseaudioFull
    pwvucontrol
    pavucontrol
    pywal
    vlc
    xwallpaper
    blueberry
    nomacs
    volumeicon
    nitrogen

    # System Utilities
    (pkgs.linuxPackages.cpupower or pkgs.cpupower) # Correct way to reference this on different kernels
    power-profiles-daemon
    mission-center
    bitwarden-desktop
    bleachbit
    arandr
    conky
    lm_sensors
    gtk3
    pkgs.gnome-settings-daemon
    gnome-calendar
    gnome-disk-utility
    mate.engrampa
    polkit_gnome
    swtpm
    sysstat
    xclip
    polybar
    xmobar
    picom
    picom-jonaburg
    pkgs.xdotool 
    redshift
    dmenu
    timeshift
    nwg-look
    pkgs.haskellPackages.xmonad-contrib
    rofi
    libnotify
    solaar
    sxhkd
    pamixer
    unzip
    xorg.xev
    xfce.xfce4-power-manager
    xfce.xfce4-screenshooter

    # Terminal Utilities
    alacritty
    btop
    fastfetch
    fzf
    kitty
    starship
    wezterm
    zsh

    # Flatpak Support
    flatpak

    # Notifications
    dunst

    # Other Utilities
    copyq
    qbittorrent
    vdhcoapp
    wget
    yt-dlp
    galculator
  ];

  # Fonts
  fonts.packages = with pkgs; [
    corefonts
    vistafonts
    inconsolata
    terminus_font
    proggyfonts
    dejavu_fonts
    font-awesome
    ubuntu_font_family
    source-code-pro
    source-sans-pro
    source-serif-pro
    noto-fonts-emoji
    openmoji-color
    twemoji-color-font
    udev-gothic-nf
    texlivePackages.inconsolata-nerd-font
    noto-fonts
    noto-fonts-color-emoji
    noto-fonts-cjk-sans
    material-icons
    maple-mono.NF
    orbitron
    fira-code
    fira-code-symbols
    jetbrains-mono
    victor-mono
    nerd-fonts.fira-code
    nerd-fonts.droid-sans-mono
    nerd-fonts.jetbrains-mono
    nerd-fonts.meslo-lg
    nerd-fonts.hack
    nerd-fonts.fantasque-sans-mono
  ];
}
