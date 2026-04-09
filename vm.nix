{ config, pkgs, ... }:
{
  # Required for virt-manager settings to persist
  programs.dconf.enable = true;

  # Best way to install virt-manager with proper desktop integration
  programs.virt-manager.enable = true;

  environment.systemPackages = with pkgs; [
    virt-viewer
    spice-gtk
    adwaita-icon-theme
    thunar
    thunar-archive-plugin
    thunar-volman
  ];

  # === Main libvirtd configuration for Windows 11 Pro ===
  virtualisation.libvirtd = {
    enable = true;

    onBoot = "start";
    onShutdown = "shutdown";

    # Run QEMU as your regular user (more secure + better performance/integration)
    qemu.runAsRoot = false;

    # Software TPM 2.0 — **required** for Windows 11
    qemu.swtpm.enable = true;

    # UEFI + Secure Boot firmware is now available by default (no ovmf block needed)
  };

  # Enable USB redirection (pass USB devices to the VM easily)
  virtualisation.spiceUSBRedirection.enable = true;

  # === Fixes for the credential/encryption key errors you saw ===
  # This makes the service more resilient when it fails to generate the key
  systemd.services.virt-secret-init-encryption = {
    serviceConfig = {
      Restart = "on-failure";
      RestartSec = "3s";
      TimeoutStartSec = "15s";   # Give it more time on slow boots
    };
  };

  # Ensure the secrets directory always exists with correct permissions
  systemd.tmpfiles.rules = [
    "d /var/lib/libvirt/secrets 0755 root root -"
  ];
}