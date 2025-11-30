{ pkgs, ... }:

{
  # System-wide packages to install.
  environment.systemPackages = with pkgs; [
    # General Applications
    p3x-onenote
    whatsapp-for-linux
    whatsie
    
    # Browsers
    firefox
    vivaldi 
    microsoft-edge

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
    totem
    mpv
    xwallpaper
    blueberry
    nomacs
    volumeicon
    nitrogen

    # System Utilities
    (pkgs.linuxPackages.cpupower or pkgs.cpupower) # Correct way to reference this on different kernels
    power-profiles-daemon
    mission-center
    cpu-x
    bitwarden-desktop
    bleachbit
    arandr
    conky
    kdiskmark
    gnome-weather
    lm_sensors
    localsend
    flameshot
    gthumb
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
    speedtest
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

    # ZSH
    zsh-autosuggestions
    zsh-syntax-highlighting
    zsh-fast-syntax-highlighting
    zsh-autocomplete
    
    # Other Utilities
    copyq
    libsForQt5.okular
    qbittorrent
    vdhcoapp
    wget
    yt-dlp
    galculator
  ];
}
