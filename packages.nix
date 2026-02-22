{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [

    # ─── Core CLI / Shell ─────────────────────────
    zsh
    starship
    fzf
    btop
    fastfetch
    unzip
    wget
    git
    ripgrep
    neovim
    yt-dlp
    xclip
    kitty
    psmisc        # killall
    procps        # pgrep, pkill
    vscode

    # ─── Web Browsers ─────────────────────────────
    firefox
    vivaldi
    microsoft-edge

    # ─── Window Manager / Desktop Core ────────────
    dmenu
    rofi
    polybar
    xmobar
    sxhkd
    dunst
    libnotify
    xdotool
    xev
    arandr
    nwg-look
    xwallpaper
    nitrogen
    pywal
    feh

    picom-jonaburg

    # ─── Session / Autostart Helpers ──────────────
    polkit_gnome
    copyq
    volumeicon
    blueberry
    flameshot
    conky

    # ─── File Management ──────────────────────────
    thunar
    mate.engrampa

    # ─── Networking / Sync ────────────────────────
    rclone
    rclone-browser
    networkmanagerapplet

    # ─── Audio / Hardware ─────────────────────────
    alsa-utils
    pavucontrol
    pamixer
    brightnessctl
    lm_sensors
    solaar
    cava

    # ─── Performance / Monitoring ─────────────────
    (linuxPackages.cpupower or pkgs.cpupower)
    sysstat
    redshift
    mission-center
    nvme-cli
    xfce4-power-manager

    # ─── Nix / System Tooling ─────────────────────
    cachix
    p3x-onenote
    python3
  ];
}
