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
    bc            # CLI calculator

    # ─── Web Browsers ─────────────────────────────
    firefox
    vivaldi
    microsoft-edge

    # ─── Window Manager / Desktop Core ────────────
    dmenu
    rofi
    # UPDATED: Polybar with PulseAudio support for PipeWire compatibility
    (polybar.override {
      pulseSupport = true;
    })
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
    blueman
    flameshot
    conky

    # ─── File Management ──────────────────────────
    thunar
    thunar-archive-plugin
    thunar-volman
    engrampa

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
