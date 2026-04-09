{ config, pkgs, lib, ... }:

{
  # ⚙️ Bash configuration (Fallback shell)
  programs.bash = {
    # Declarative aliases
    shellAliases = {
      # Nix Management
      fu = "(cd /etc/nixos && sudo nixos-rebuild switch --flake .#K10)";
      fub = "(cd /etc/nixos && nix build .#nixosConfigurations.K10.config.system.build.toplevel)";
      up = "sudo /home/j3ll0/nix_update.sh";
      dot = "cd /etc/nixos/";

      # System & Editor
      ff = "clear && fastfetch"; 
      trim = "sudo fstrim -v /";
      ls = "ls --color=auto";
      grep = "grep --color=auto";
      v = "sudo -E nvim"; 
      keys = ".config/scripts/update_xmonad_keys.sh";
    };

    # shellInit is the correct option for Bash initialization code
    shellInit = ''
      # ============================================================================
      # SHELL OPTIONS
      # ============================================================================
      stty -ixon
      shopt -s histappend
      shopt -s checkwinsize
      shopt -s cdspell
      shopt -s dirspell
      shopt -s autocd
      shopt -s globstar
      shopt -s nocaseglob
      shopt -s extglob

      # ============================================================================
      # HISTORY CONFIGURATION
      # ============================================================================
      export HISTSIZE=10000
      export HISTFILESIZE=20000
      export HISTCONTROL=ignoreboth:erasedups
      export HISTIGNORE="ls:ll:cd:pwd:exit:clear:history"
      export HISTTIMEFORMAT="%F %T "
      export PROMPT_COMMAND="history -a; history -c; history -r; $PROMPT_COMMAND"

      # ============================================================================
      # ENVIRONMENT VARIABLES (Unified Path)
      # ============================================================================
      export PATH="$HOME/scripts:$HOME/.local/bin:/usr/local/go/bin:$HOME/.cargo/bin:$PATH"
      export EDITOR=$(command -v nvim || command -v vim || command -v micro || echo nano)
      export VISUAL="$EDITOR"
      export LESS='-R -F -X -i -P %f (%i/%m) '
      export LESSHISTFILE=/dev/null
      export LESS_TERMCAP_mb=$'\e[1;32m'
      export LESS_TERMCAP_md=$'\e[1;32m'
      export LESS_TERMCAP_me=$'\e[0m'
      export LESS_TERMCAP_se=$'\e[0m'
      export LESS_TERMCAP_so=$'\e[01;33m'
      export LESS_TERMCAP_ue=$'\e[0m'
      export LESS_TERMCAP_us=$'\e[1;4;31m'
      export LANG=en_US.UTF-8
      export LC_ALL=en_US.UTF-8

      # ============================================================================
      # WELCOME MESSAGE (Bash-specific)
      # ============================================================================
      if [ -n "$PS1" ]; then
        echo -e "\033[1;34m=== Welcome back, $USER! ===\033[0m"
        echo -e "Date: $(date '+%A, %B %d, %Y - %I:%M %p')"
        echo -e "Uptime: $(uptime | awk '{print $3}' | sed 's/,//')"
        echo -e "Load: $(uptime | awk -F'load average:' '{print $2}')"
        echo
      fi
      # ⚙️ Starship configuration
      ${lib.optionalString config.programs.starship.enable "eval \"$(starship init bash)\""}
    '';
  };

  # ⚙️ Zsh configuration (Default shell)
  programs.zsh = {
    enable = true;
    shellAliases = {
      # Nix Management
      fu = "(cd /etc/nixos && sudo nixos-rebuild switch --flake .#K10)";
      fub = "(cd /etc/nixos && nix build .#nixosConfigurations.K10.config.system.build.toplevel)";
      up = "sudo /home/j3ll0/nix_update.sh";
      dot = "cd /etc/nixos/";

      # System & Editor
      ff = "clear && fastfetch"; 
      ll = "ls -l"; 
      trim = "sudo fstrim -v /";
      ls = "ls --color=auto";
      grep = "grep --color=auto";
      v = "sudo -E nvim";
      keys = ".config/scripts/update_xmonad_keys.sh";
    };

    # CRITICAL FALLBACK: Using the core 'shellInit' option and manual sourcing
    shellInit = ''
      # ============================================================================
      # OH-MY-ZSH MANUAL SOURCING 
      # ============================================================================
      # Set ZSH path and theme, relying on 'oh-my-zsh' being in environment.systemPackages
      export ZSH="${pkgs.oh-my-zsh}/share/oh-my-zsh"
      ZSH_THEME="robbyrussell" 
      DISABLE_AUTO_UPDATE="true"
      
      # Source the Oh My Zsh main script
      source $ZSH/oh-my-zsh.sh

      # ============================================================================
      # ZSH PLUGIN SOURCING (Manual configuration)
      # ============================================================================
      
      # ⚙️ Starship configuration: Must be initialized after OMZ
      ${lib.optionalString config.programs.starship.enable "eval \"$(starship init zsh)\""}

      # Source zsh-autosuggestions
      source ${pkgs.zsh-autosuggestions}/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
      
      # Source zsh-syntax-highlighting
      source ${pkgs.zsh-syntax-highlighting}/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
      
      # Source zsh-autocomplete
      source ${pkgs.zsh-autocomplete}/share/zsh/plugins/zsh-autocomplete/zsh-autocomplete.plugin.zsh

      # ============================================================================
      # WELCOME MESSAGE 
      # ============================================================================
      if [ -n "$PS1" ]; then
          echo -e "\033[1;34m=== Welcome back, $USER! ===\033[0m"
          echo -e "Date: $(date '+%A, %B %d, %Y - %I:%M %p')"

          # RAM usage
          ram_used=$(free -h | awk '/Mem:/ {print $3}')
          ram_total=$(free -h | awk '/Mem:/ {print $2}')
          echo -e "RAM Usage: $ram_used / $ram_total"

          # CPU temperature (suppress error line)
          cpu_temp=$(sensors 2>/dev/null | grep 'Package id 0:' | awk '{print $4}')
          echo -e "CPU Temp: $cpu_temp"

          echo -e "type 'alias' to show aliases"
          echo
      fi
    '';
  };
  
  # ⚙️ Starship configuration
  programs.starship.enable = true;

  # Add zsh to the list of system shells
  environment.shells = with pkgs; [
    zsh
    bash
  ];
  
  # 📦 Add all required packages (including shell components) to the system environment
  environment.systemPackages = with pkgs; [
    zsh-autosuggestions
    zsh-syntax-highlighting
    zsh-autocomplete
    oh-my-zsh
    # FIX: Use lm_sensors for the 'sensors' command utility
    lm_sensors
  ];
}
