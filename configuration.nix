{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ./packages.nix
    ./services.nix
    ./update.nix
    ./video-accel.nix
    ./maintenance.nix
    ./vm.nix
    ./starfish.nix
    # ./xmonad.nix  # Optional: enable for declarative XMonad
  ];

  # 🖥️ System Identity
  networking.hostName = "K10";
  time.timeZone = "America/Toronto";
  i18n.defaultLocale = "en_CA.UTF-8";

  # 🧬 Bootloader & Kernel
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelParams = [ "mitigations=off" ];
  boot.kernel.sysctl."vm.dirty_ratio" = 20;

  # 🧠 Firmware & Microcode
  hardware.cpu.intel.updateMicrocode = true;
  hardware.enableRedistributableFirmware = true;
  services.thermald.enable = true;

  # 🎮 Graphics Stack
  hardware.graphics.enable = true;
  hardware.graphics.enable32Bit = true;

  # 💾 Swap & Temp
  zramSwap.enable = true;

  fileSystems."/tmp" = {
    device = "tmpfs";
    fsType = "tmpfs";
    options = [ "size=2G" "mode=1777" ];
  };

  fileSystems."/mnt/data" = {
    device = "/dev/disk/by-uuid/0618d31f-4e05-4fc0-8db1-9d62bebab4d0";
    fsType = "ext4";
    options = [ "defaults" "nofail" "x-systemd.automount" ];
    neededForBoot = false;
  };

  # 🧪 Nix Settings
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # 👤 User Configuration
  users.users.j3ll0 = {
    isNormalUser = true;
    description = "Angelo";
    extraGroups = [
      "wheel" "networkmanager" "libvirtd"
      "audio" "video" "input" "kvm"
    ];
    packages = [ ]; # Add user-specific packages here
  };

  # 🔒 Security
  security.polkit.enable = true;
  security.sudo.enable = true;
  security.sudo.extraConfig = ''
    Defaults env_reset,pwfeedback
  '';

  # ⚙️ Input & Desktop Services
  programs.xfconf.enable = true;
  services.libinput.enable = true;

  # 🖥️ Display Manager & X Server
  services.displayManager.sddm.enable = true;
  services.xserver.enable = true;

  # 🪟 XMonad Window Manager
  services.xserver.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    extraPackages = haskellPackages: [
      haskellPackages.utf8-string
    ];
  };

  # 📂 Thunar File Manager
  programs.thunar = {
    enable = true;
    plugins = with pkgs.xfce; [
      thunar-archive-plugin
      thunar-volman
    ];
  };

  # 🌐 XDG Portal (for Flatpak, sandboxed apps)
  xdg.portal = {
    enable = true;
    extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
    config.common.default = "*"; # Fixes portal fallback warnings
  };

  # 🔋 Power Management
  services.power-profiles-daemon.enable = true;
  services.upower.enable = true; # Fixes xfce4-power-manager warning
  powerManagement.cpuFreqGovernor = "performance";

  # 📦 Nixpkgs Config (only used if pkgs not passed via flake)
  # nixpkgs.config.allowUnfree = true;

  # 🧭 System Version
  system.stateVersion = "25.05";
}
