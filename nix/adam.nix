{ config, pkgs, myDict, myTaffybar, ... }:

let
myHaskellEnv = pkgs.haskell.packages.ghc802.ghcWithPackages (
  haskellPackages: with haskellPackages; [
  aeson hlint lens lens-xml mustache recursion-schemes taffybar reactive-banana xml
]);
in

{
    isNormalUser = true;
    packages = with pkgs; [
      baobab
      base16
      binutils
      davmail
      dropbox
      feh
      file
      firefox
      gimp
      git
      gitAndTools.hub
      glibc.static
      gnupg
      graphviz
      myHaskellEnv
      # myWebHaskellEnv
      hunspell
      myDict
      jre
      julia
      ledger
      libreoffice
      myTaffybar
      nixops
      # nodePackages.bower
      nodePackages.eslint
      nodePackages.jshint
      muchsync
      nix-prefetch-git
      notmuch
      offlineimap
      openssl
      pass
      pidgin-with-plugins
      (python27Full.buildEnv.override {
        extraLibs = with python27Packages; [ flake8 ipython pylint pyparsing html5lib reportlab lxml numpy scipy sphinx h5py pyopencl matplotlib wxPython pyqt5];
        ignoreCollisions = true;
      })
      proselint
      ripgrep
      # sasview
      super-user-spark
      texlive.combined.scheme-full
      tightvnc
      tmux
      unzip
      xfce.thunar
      zathura
      zip];
}
