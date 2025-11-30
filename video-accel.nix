{ config, pkgs, ... }:

{
  # 🎮 Graphics acceleration and driver support
  hardware.graphics = {
    enable = true; # Enables OpenGL, pulls in the core 'mesa' package (which contains the Intel Vulkan driver)

    # 🚀 MEDIA ACCELERATION (VAAPI/VDPAU)
    extraPackages = with pkgs; [
      intel-media-driver        # VAAPI driver for Gen9+ Intel GPUs
      libvdpau-va-gl            # VDPAU-to-VAAPI bridge
      
      # VULKAN SUPPORT
      vulkan-loader             # Standard Vulkan loader
      # NOTE: Removed 'vulkan-intel' because it was causing an "undefined variable" error. 
      # The driver is provided implicitly by 'mesa' due to hardware.graphics.enable = true.
    ];
  };

  # 🧩 32-bit GPU support for legacy apps (Steam, Wine)
  # We use the general 32-bit 'mesa' package to ensure all 32-bit GL/Vulkan drivers are present.
  environment.systemPackages = with pkgs; [
    # 32-bit Media Acceleration
    pkgsi686Linux.intel-media-driver 
    
    # 32-bit Graphics Drivers (Vulkan/OpenGL)
    pkgsi686Linux.mesa
    pkgsi686Linux.vulkan-loader
  ];

  # 🌐 Environment variables for media acceleration
  environment.variables = {
    LIBVA_DRIVER_NAME = "iHD";      # Use intel-media-driver for VAAPI
    VDPAU_DRIVER = "va_gl";         # Route VDPAU apps through VAAPI bridge
  };
}
