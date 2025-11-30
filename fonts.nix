{ pkgs, ... }:

{
  # This module declares all system-wide fonts.
  fonts.packages = with pkgs; [

    # Core Fonts and General Use
    dejavu_fonts
    noto-fonts
    noto-fonts-cjk-sans 
    ubuntu_font_family
    source-code-pro
    source-sans-pro
    source-serif-pro

    # Coding Fonts (Unpatched)
    inconsolata
    fira-code 
    jetbrains-mono
    victor-mono

    # Emoji & Icon Fonts
    font-awesome_6 
    material-icons
    noto-fonts-color-emoji
    noto-fonts-emoji
    openmoji-color
    twemoji-color-font

    # ✅ Nerd Fonts (Using the fixed, individual package names)
    nerd-fonts.hack
    nerd-fonts.fira-code
    nerd-fonts.jetbrains-mono
    nerd-fonts.meslo-lg  # <-- FIX: Changed 'meslo' to 'meslo-lg'
    udev-gothic-nf 

    # Specific/Legacy Fonts
    dina-font        
    terminus_font
    proggyfonts
    orbitron         

    # Windows Compatibility Fonts (Only if needed)
    corefonts
    vistafonts
  ];
}