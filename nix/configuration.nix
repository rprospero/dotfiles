# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  nixpkgs.config.allowUnfree = true;

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "uk";
    defaultLocale = "en_GB.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "Europe/London";

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    emacs sudo git openssh tmux zsh firefox gnupg dbus stack ghc taffybar haskellPackages.taffybar ghc lightdm oh-my-zsh dropbox mesa
  ];

  fonts = {
	fonts = with pkgs; [
	      dejavu_fonts
	      source-code-pro
	      source-sans-pro
	      source-serif-pro
	      inconsolata
	];
  };

  programs.zsh.enable = true;

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable the X11 windowing system.
  services.xserver = {
		   enable = true;
		   layout = "uk";
		   windowManager.xmonad = {
					enable = true;
					enableContribAndExtras = true;
      extraPackages = haskellPackages: [
	haskellPackages.xmonad-contrib
	haskellPackages.xmonad-extras
	haskellPackages.xmonad
	haskellPackages.taffybar
      ];
    };
    windowManager.default = "xmonad";
  };

  # Enable the KDE Desktop Environment.
  services.xserver.displayManager.lightdm.enable = true;
  # services.xserver.desktopManager.plasma5.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.adam = {
    isNormalUser = true;
    uid = 1000;
    home = "/home/adam";
    description = "Adam Washington";
    extraGroups = ["wheel" "vboxusers" "vboxsf"];
  };


  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "17.03";

}
