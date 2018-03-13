# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
icon-fonts-personal = pkgs.haskellPackages.callPackage ./icon-fonts.nix {};
myTaffybar = pkgs.taffybar.override {
  packages = x: with pkgs.haskellPackages; [
    aeson download hostname icon-fonts-personal yaml];
};
myDict = pkgs.hunspellDicts.en-gb-ise.overrideAttrs (old: rec {
  preFixup = ''
    ln -sv $out/share/hunspell/en_GB-ise.aff $out/share/hunspell/en_GB.aff
    ln -sv $out/share/hunspell/en_GB-ise.dic $out/share/hunspell/en_GB.dic
  '';
  phases = "unpackPhase installPhase fixupPhase";
});
in

{
  imports =
    [ # Include the results of the hardware scan.
      /etc/nixos/hardware-configuration.nix
      /etc/nixos/local.nix
    ];

  # nix.binaryCaches = [ "https://cache.nixos.org/" "https://nixcache.reflex-frp.org" ];
  # nix.binaryCachePublicKeys = [ "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" ];

  # networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "uk";
    defaultLocale = "en_GB.UTF-8";
  };


  # Set your time zone.
  time.timeZone = "Europe/London";

  nixpkgs.config = {
    allowUnfree = true;
    packageOverrides = pkgs: with pkgs; {
      pidgin-with-plugins = pkgs.pidgin-with-plugins.override {
        plugins = [ pidginotr pidgin-skypeweb telegram-purple purple-matrix];
      };
    };
  };


  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    wget vim emacs rxvt_unicode
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.bash.enableCompletion = true;
  # programs.mtr.enable = true;
  programs.gnupg.agent = { enable = true; enableSSHSupport = true; };
  programs.fish.enable = true;

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  services.emacs.enable = true;
  services.gnome3.at-spi2-core.enable = true;
  services.keybase.enable = true;
  services.kbfs.enable = true;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.layout = "gb";
  # services.xserver.xkbOptions = "eurosign:e";

  # Enable touchpad support.
  # services.xserver.libinput.enable = true;

  # Enable the KDE Desktop Environment.
  # services.xserver.displayManager.sddm.enable = true;
  # services.xserver.desktopManager.plasma5.enable = true;

  services.xserver.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    extraPackages = haskellPackages: [
      haskellPackages.taffybar
    ];
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.adam = (import /home/adam/Code/dotfiles/nix/adam.nix) {inherit config pkgs myDict myTaffybar;};
  environment.variables.DICPATH = "${myDict}/share/hunspell";

  fonts.fonts = with pkgs; [
    dejavu_fonts
    emacs-all-the-icons-fonts
    iosevka
  ];

  users.defaultUserShell = pkgs.fish;

  systemd.user.services.offlineimap = {
    description = "Offline Imap Daemon";
    serviceConfig = {
      ExecStart="${pkgs.offlineimap}/bin/offlineimap";
      Restart="on-failure";
      RestartSec=3;
    };
    path = [pkgs.offlineimap pkgs.notmuch pkgs.gnupg];
    enable = true;
    requires = ["gpg-agent.service" "davmail.service"];
  };

  systemd.user.services.taffybar = {
    description = "Taffybar Status Bar";
    serviceConfig = {
      ExecStart="${myTaffybar}/bin/taffybar";
      Restart = "on-failure";
    };
    path = [myTaffybar pkgs.notmuch];
  };

  systemd.user.services.davmail = {
    description = "Davmail Daemon";
    serviceConfig = {
      ExecStart="${pkgs.davmail}/bin/davmail";
      Restart = "on-failure";
    };
    path = [pkgs.davmail];
    requires = ["taffybar.service"];
    enable = true;
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "17.09"; # Did you read the comment?

}
