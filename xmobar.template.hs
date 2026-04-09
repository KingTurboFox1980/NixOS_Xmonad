-- ~/.config/xmobar/xmobar.template.hs
Config { font = "Dina Bold 10"
       , additionalFonts = [...]
       , bgColor = "$wal_bg"
       , fgColor = "$wal_fg"
       , position = BottomSize L 100 25
       , alpha = 200
       , iconRoot = "~/.xmonad/xpm/"
       , commands = [...]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "$wal_template"
       }
