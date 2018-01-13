{ config, pkgs, ... }:

{
  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  # boot.loader.grub.efiSupport = true;
  # boot.loader.grub.efiInstallAsRemovable = true;
  # boot.loader.efi.efiSysMountPoint = "/boot/efi";
  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/sda"; # or "nodev" for efi only

  users.extraUsers.adam.extraGroups = ["wheel" "vboxuser" "vboxsf"];
  users.extraUsers.adam.uid = 1000;

  users.extraUsers.stacy.extraGroups = ["wheel" "vboxuser" "vboxsf"];
  users.extraUsers.stacy.uid = 1001;

  fileSystems."/Documents" = {
    fsType = "vboxsf";
    device = "Documents";
    options = ["rw"];
  };

  virtualisation.virtualbox.guest.enable = true;
}