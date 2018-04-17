{ config, pkgs, myDict, myTaffybar, ... }:

let
icon-fonts-personal = pkgs.haskellPackages.callPackage ./icon-fonts.nix {};
myHaskellEnv = pkgs.haskellPackages.ghcWithHoogle (
  haskellPackages: with haskellPackages; [
  aeson hlint icon-fonts-personal lens lens-xml mustache recursion-schemes reactive-banana xml yaml
]);
in

{
    isNormalUser = true;
    packages = with pkgs; [
      baobab
      base16-builder
      binutils
      davmail
      dropbox
      faba-icon-theme
      feh
      file
      firefox
      gcc
      gimp
      git
      gitAndTools.hub
      glibc.static
      gnumake
      gnupg
      graphviz
      # myHaskellEnv
      # myWebHaskellEnv
      hunspell
      myDict
      keepassx-community
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
      mypy
      nix-prefetch-git
      notmuch
      offlineimap
      openssl
      pass
      pidgin-with-plugins
      (python27Full.buildEnv.override {
        extraLibs = with python27Packages; [ flake8 ipython jupyter lark-parser mock pylint pyparsing html5lib reportlab lxml numpy scipy sphinx sympy h5py pytest pyopencl matplotlib wxPython pyqt5];
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
