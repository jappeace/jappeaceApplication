TITLE: Nixos notes
DATE: 2019-04-18
CATEGORY: tools
Tags: pain, devops, virtualization, tools, linux
subreddit: nixos

This is a post of things I wanted to do in nixos
but isn't described anywhere.
I had to read source code to figure these things out.
By explaining here what is going on I make things easier for other people.

# Nix custom image
It's possible to bypass virtual box and make a bunch of different image
formats directly, I used two files `image.nix`:

```nix
{ config, ... }:

let
  pkgs = import ./pin.nix { };
in

{ 
  system.build.image = import <nixpkgs/nixos/lib/make-disk-image.nix> {
      name = "nixos-vmdk-${config.system.nixos.label}-${pkgs.stdenv.hostPlatform.system}";
      format = "vpc";
      inherit pkgs config;
      lib = pkgs.lib;
      partitionTableType = "legacy";
      diskSize = 11 * 1024;
  };
  .... # remaining config, same as configuration.nix
}
```

and the `disk.nix`:
```nix
{ nixos ? <nixpkgs/nixos>
, system ? builtins.currentSystem
}:

let
  machine-configuration = import ./image.nix;
  machine = import nixos {
    inherit system;
    configuration = machine-configuration;
  };
in 
machine.config.system.build.image
```

If you run `nix-build disk.nix` you'll get a VHD with the configuration from `image.nix`
which is just a nixos standard `configuraiton.nix`.

# Nixos-rebuild remote
We can use nixos-rebuild to do in place updates of a running system remotely.
If your deployment is a single VM this is significantly easier than using
nixops.
I ended up with this make file:

```shell
IP="192.168.0.39"
deploy: 
	NIXOS_CONFIG=$(shell pwd)"/image.nix" nixos-rebuild switch --target-host root@$(IP)
```

You may also want to set `--build-host`, because by default it will build on `target-host`.

# Install nix on running nix
For some reason my boot disk gets corrupted after switching a couple of times.
A solution is just to never reboot, however
you can also just fix this while running the system.
Switching doesn't do this apparently.
I know this because it didn't boot.
This doesn't matter because you can install your currently running system!

Mount root as `/mnt`.
Format your boot partition and mount it on `/mnt/boot`.
Then run `nixos-install`.
For example:

```shell
mount # list everything mounted
mount /dev/nvme0n1p2 /mnt
umount /boot
mkfs.vfat -n boot /dev/nvme0n1p1
mount /dev/nvme0n1p1 /boot
mount /dev/nvme0n1p1 /mnt/boot
```
