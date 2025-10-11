{ config, pkgs, ... }:

{
  # 🎮 Graphics acceleration and driver support
  hardware.graphics = {
    enable = true; # Enables OpenGL and GPU acceleration
    extraPackages = with pkgs; [
      intel-media-driver     # VAAPI driver for Gen9+ Intel GPUs (e.g. 13th Gen)
      libvdpau-va-gl         # VDPAU-to-VAAPI bridge for compatibility
    ];
  };

  # 🧩 Optional: 32-bit GPU support for legacy apps (Steam, Wine)
  environment.systemPackages = with pkgs; [
    pkgsi686Linux.intel-media-driver # 32-bit VAAPI driver for compatibility
  ];

  # 🌐 Environment variables for media acceleration
  environment.variables = {
    LIBVA_DRIVER_NAME = "iHD";  # Use intel-media-driver for VAAPI
    VDPAU_DRIVER = "va_gl";     # Route VDPAU apps through VAAPI bridge
  };
}
