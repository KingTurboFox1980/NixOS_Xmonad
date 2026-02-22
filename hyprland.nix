{ config, pkgs, ... }:

{
  # Enable Hyprland (Wayland compositor)
  programs.hyprland = {
    enable = true;
    xwayland.enable = true;
  };

  # Polkit (sudo dialogs, thunar, gnome-disks, nm-applet, etc.)
  security.polkit.enable = true;

  # XDG portals (REQUIRED for Wayland apps)
  xdg.portal = {
    enable = true;
    xdgOpenUsePortal = true;
    extraPortals = [ pkgs.xdg-desktop-portal-hyprland ];
  };

  # Disable X11
  services.xserver.enable = false;

  # Wayland environment variables
  environment.sessionVariables = {
    NIXOS_OZONE_WL = "1";
    MOZ_ENABLE_WAYLAND = "1";
    QT_QPA_PLATFORM = "wayland";
    XDG_SESSION_TYPE = "wayland";
  };
}
