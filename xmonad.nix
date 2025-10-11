{ config, pkgs, lib, ... }:

let
  my-xmonad-config = builtins.readFile ./xmonad.hs;
in
{
  # Enable the X server.
  services.xserver = {
    enable = true;
    # Tell X to use XMonad as the window manager.
    displayManager.defaultSession = "none+xmonad";
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = my-xmonad-config;
    };

    # This is the correct way to start applications like sxhkd, polybar, picom, and dunst.
    displayManager.setupCommands = ''
      ${pkgs.sxhkd}/bin/sxhkd -c /home/j3ll0/.config/sxhkd/sxhkdrc &
      /home/j3ll0/.config/polybar/launch.sh &
      ${pkgs.picom}/bin/picom --config /home/j3ll0/.config/picom/picom.conf &
      ${pkgs.dunst}/bin/dunst --config /home/j3ll0/.config/dunst/dunstrc &
    '';
  };

  # Configure your user's applications and environment.
  environment.systemPackages = with pkgs; [
    alacritty
    sxhkd
    polybar
    picom
    dunst
    xfce.thunar
    xfce.thunar-archive-plugin
    xfce.thunar-volman
  ];
}
