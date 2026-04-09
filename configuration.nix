{ config, pkgs, lib, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ./cachix.nix
    ./fonts.nix
    ./maintenance.nix
    ./packages.nix
    ./services.nix
    ./starfish.nix
    ./update.nix
    ./video-accel.nix
    ./vm.nix
    # ./xmonad.nix
  ];

  # ─────────────────────────────────────────────
  # 🖥️ System Identity
  # ─────────────────────────────────────────────
  networking.hostName = "K10";
  time.timeZone = "America/Toronto";
  i18n.defaultLocale = "en_CA.UTF-8";

  # ─────────────────────────────────────────────
  # 🧬 Bootloader & Kernel
  # ─────────────────────────────────────────────
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.kernelParams = [ "mitigations=off" ];
  boot.kernel.sysctl = {
    "vm.dirty_ratio" = 20;
  };

  # ─────────────────────────────────────────────
  # 🧠 Firmware & Microcode
  # ─────────────────────────────────────────────
  hardware.cpu.intel.updateMicrocode = true;
  hardware.enableRedistributableFirmware = true;
  services.thermald.enable = true;

  # ─────────────────────────────────────────────
  # 🎮 Graphics
  # ─────────────────────────────────────────────
  hardware.graphics = {
    enable = true;
    enable32Bit = true;
  };

  # ─────────────────────────────────────────────
  # 🔊 Audio / RT
  # ─────────────────────────────────────────────
  security.rtkit.enable = true;

  # ─────────────────────────────────────────────
  # 💾 Swap / Temp
  # ─────────────────────────────────────────────
  boot.tmp.useTmpfs = false;
  zramSwap.enable = true;

  # ─────────────────────────────────────────────
  # 📁 Extra Filesystems
  # ─────────────────────────────────────────────
  fileSystems."/mnt/data" = {
    device = "/dev/disk/by-uuid/0618d31f-4e05-4fc0-8db1-9d62bebab4d0";
    fsType = "ext4";
    options = [ "defaults" "nofail" "x-systemd.automount" ];
  };

  # ─────────────────────────────────────────────
  # 🧪 Nix / Flakes
  # ─────────────────────────────────────────────
  nix.settings = {
    experimental-features = [ "nix-command" "flakes" ];
    auto-optimise-store = true;
  };

  # ─────────────────────────────────────────────
  # 👤 User
  # ─────────────────────────────────────────────
  users.users.j3ll0 = {
    isNormalUser = true;
    description = "Angelo";
    shell = pkgs.zsh;
    extraGroups = [
      "wheel"
      "networkmanager"
      "libvirtd"
      "audio"
      "video"
      "input"
      "kvm"
    ];
  };

  # ─────────────────────────────────────────────
  # 🔒 Security
  # ─────────────────────────────────────────────
  security.sudo = {
    enable = true;
    extraConfig = ''
      Defaults env_reset,pwfeedback
    '';
  };

  # ─────────────────────────────────────────────
  # 🐚 Shell
  # ─────────────────────────────────────────────
  programs.zsh.enable = true;

  # ─────────────────────────────────────────────
  # ⚙️ Input & Desktop
  # ─────────────────────────────────────────────
  programs.xfconf.enable = true;
  services.libinput.enable = true;

# ─────────────────────────────────────────────
# 🖥️ Display / X11
# ─────────────────────────────────────────────

services.xserver = {
  enable = true;

  xkb = {
    layout = "us";
    options = "caps:escape";
  };

  displayManager.lightdm = {
    enable = true;

    greeters.gtk = {
      enable = true;
      theme.name = "Adwaita-dark";
    };
  };

  windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;

    extraPackages = hp: [
      hp.utf8-string
    ];
  };
};

# ⬇️ Renamed option (moved out of xserver namespace)
services.displayManager.defaultSession = "none+xmonad";


  # ─────────────────────────────────────────────
  # 🧭 State Version
  # ─────────────────────────────────────────────
  system.stateVersion = "25.05";
}
