{ config, pkgs, lib, ... }:

{
  # ⚙️ Bash configuration
  programs.bash = {
    enable = true;
    
    # Declarative aliases
    shellAliases = {
      ff = "fastfetch";
    };

    # All other shell options and environment variables
    # This will be written to the generated .bashrc
    initExtra = ''
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
      # ENVIRONMENT VARIABLES
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
      # WELCOME MESSAGE
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

  # ⚙️ Zsh configuration
  programs.zsh = {
    enable = true;
    shellAliases = {
      ff = "fastfetch";
      ll = "ls -l";
    };
    ohMyZsh = {
      enable = true;
      plugins = [ "git" ];
    };
    plugins = [
      "zsh-autosuggestions"
    ];
    autosuggestion = {
      enable = true;
    };
    syntaxHighlighting = {
      enable = true;
    };
    extraPackages = [ pkgs.zsh-autocomplete ];
    initExtra = ''
      # ⚙️ Starship configuration
      ${lib.optionalString config.programs.starship.enable "eval \"$(starship init zsh)\""}
    '';
  };
  
  # ⚙️ Starship configuration
  programs.starship.enable = true;

  # Make zsh the default shell for your user
  users.users.j3ll0.shell = pkgs.zsh;

  # Add zsh to the list of system shells
  environment.shells = with pkgs; [
    zsh
    bash
  ];
}
