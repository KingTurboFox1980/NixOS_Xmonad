{ config, pkgs, lib, ... }:

{
  # =====================================================
  # 🔧 CORE SYSTEM SERVICES
  # =====================================================
  services = {
    # ---------------------
    # 💾 Storage / File Mgmt
    # ---------------------
    udisks2.enable = true;
    gvfs.enable = true;
    tumbler.enable = true;
    fstrim.enable = true;
    devmon.enable = true;

    # ---------------------
    # 🪟 SMB / WINDOWS SHARES
    # ---------------------
    samba = {
      enable = true;
      settings.global.security = "user";
    };
    samba-wsdd.enable = true;

    # ---------------------
    # 🧠 SYSTEM HEALTH
    # ---------------------
    earlyoom = {
      enable = true;
      freeMemThreshold = 5;
      freeSwapThreshold = 5;
    };
    smartd.enable = true;
    fwupd.enable = true;
    thermald.enable = true;

    journald.extraConfig = ''
      SystemMaxUse=200M
      RuntimeMaxUse=50M
    '';

    # ---------------------
    # 🔋 POWER
    # ---------------------
    power-profiles-daemon.enable = true;
    upower.enable = true;

    # ---------------------
    # 🎧 AUDIO (PIPEWIRE)
    # ---------------------
    pipewire = {
      enable = true;
      pulse.enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      jack.enable = true;
      wireplumber.enable = true;
    };

    # ---------------------
    # 🔑 SECRETS / KEYRING
    # ---------------------
    gnome.gnome-keyring.enable = true;

    # ---------------------
    # 🖨️ PRINTING
    # ---------------------
    printing.enable = true;

    # ---------------------
    # ⏱️ TIME
    # ---------------------
    timesyncd.enable = true;

    # ---------------------
    # 📦 FLATPAK
    # ---------------------
    flatpak.enable = true;

    # ---------------------
    # 🌐 AVAHI
    # ---------------------
    avahi = {
      enable = true;
      nssmdns4 = true;
      openFirewall = true;
    };
  };

  # =====================================================
  # ⚙️ KERNEL / POWER
  # =====================================================
  boot.kernelParams = [ "intel_pstate=active" ];
  powerManagement.cpuFreqGovernor = "schedutil";
  security.rtkit.enable = true;

  # Clean /tmp on boot
  systemd.tmpfiles.rules = [
  "d /tmp 1777 root root 0d"
  ];

  systemd.services.NetworkManager-wait-online.enable = false;

  # =====================================================
  # 🧠 CPU SLICES
  # =====================================================
  systemd.slices = {
    "pcore.slice" = {
      description = "P-core high performance slice";
      sliceConfig = {
        AllowedCPUs = "0-11";
        CPUWeight = 100;
      };
    };
    "ecore.slice" = {
      description = "E-core background slice";
      sliceConfig = {
        AllowedCPUs = "12-19";
        CPUWeight = 50;
      };
    };
  };

  # Move selected services to E-cores
  systemd.services.earlyoom.serviceConfig = {
    Slice = "ecore.slice";
    Nice = 10;
  };
  systemd.services.smartd.serviceConfig = {
    Slice = "ecore.slice";
    Nice = 10;
  };
  systemd.services.fwupd.serviceConfig = {
    Slice = "ecore.slice";
    Nice = 10;
  };

  # =====================================================
  # 🧠 NVMe THERMAL MONITOR
  # =====================================================
  systemd.services.nvme-thermal-monitor = {
    description = "Monitor NVMe temperature and log";
    serviceConfig = {
      ExecStart = "${pkgs.bash}/bin/bash -c 'while true; do nvme smart-log /dev/nvme0 | grep temperature; sleep 60; done'";
      Type = "simple";
      Slice = "ecore.slice";
      Nice = 10;
    };
    wantedBy = [ "multi-user.target" ];
  };

  # =====================================================
  # 🖥️ XDG PORTALS
  # =====================================================
  xdg.portal = {
    enable = true;
    extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
    config.common.default = [ "gtk" ];
  };

  # =====================================================
  # 👮 SECURITY / POLICYKIT
  # =====================================================
  security.polkit.enable = true;

  # =====================================================
  # 🖥️ VIRTUALIZATION
  # =====================================================
  programs.virt-manager.enable = true;
  virtualisation = {
    libvirtd.enable = true;
    spiceUSBRedirection.enable = true;
  };

  # =====================================================
  # 🔥 FIREWALL
  # =====================================================
  networking.firewall = {
    enable = true;
    allowedTCPPorts = [ 139 445 ];
    allowedUDPPorts = [ 137 138 ];
  };

  # =====================================================
  # 📦 SYSTEM PACKAGES
  # =====================================================
  environment.systemPackages = with pkgs; [
    libsecret
    seahorse
    thunar
    nvme-cli
    (linuxPackages.cpupower or pkgs.cpupower)
    cifs-utils
  ];
}