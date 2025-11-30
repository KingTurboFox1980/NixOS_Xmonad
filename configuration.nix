{ config, pkgs, lib, specialArgs, ... }: 
{
  imports = [
    ./hardware-configuration.nix
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

  # 🖥️ System Identity
  networking.hostName = "K10";
  time.timeZone = "America/Toronto";
  i18n.defaultLocale = "en_CA.UTF-8";

  # 🧬 Bootloader & Kernel
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelParams = [ "mitigations=off" ]; # Performance optimization
  boot.kernel.sysctl."vm.dirty_ratio" = 20;  # Good for desktop responsiveness

  # 🧠 Firmware & Microcode
  hardware.cpu.intel.updateMicrocode = true;
  hardware.enableRedistributableFirmware = true;
  services.thermald.enable = true;

  # 🎮 Graphics Stack
  hardware.graphics.enable = true;
  hardware.graphics.enable32Bit = true;

  # 🔊 AUDIO
  security.rtkit.enable = true;

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
    packages = [ ];
    
    # Set Zsh as the default shell for the user
    shell = pkgs.zsh;
  };

  # 🔒 Security
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
  
  # ⌨️ SHELL CONFIGURATION
  # Zsh is enabled here, but all custom initialization is removed.
  programs.zsh.enable = true; 

  # ⌨️ CORE PACKAGES
  # Only core utilities and Alacritty remain. Zsh is included for clarity.
  environment.systemPackages = with pkgs; [
    zsh
    alacritty
    lm_sensors               
    coreutils                
  ];

  # 🧭 System Version
  system.stateVersion = "25.05";
}
