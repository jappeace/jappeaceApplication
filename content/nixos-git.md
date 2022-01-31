Title: Installing a NixOS desktop tracked with git
Date: 2022-01-30 16:20
Modified: 2022-02-01 01:00
Category: tools
OPTIONS: toc:nil
Tags: nix, nixos, ext4, tools, linux, devops

A few years ago I wrote a post on installing
NixOS [on encrypted btrfs]({filename}/nixos-encrypted-btrfs.md).
I recently went trough that guide to install
NixOS once more.
It is good, but it has some issues:

1. btrfs: Which I no longer use due to performance concerns.
2. [git](https://git-scm.com/): This requires some special attention,
   but composes really well with nix and nixos.
3. It doesn't explain how to deal with secrets.

I'll address these concerns here,
since I *just* bricked an install due to git usage [^hardware].
And I also wasted some time re-figuring out secrets.

Why use git if it introduces complexity?
For one it serves as an excellent backup tool,
furthermore it allows managing multiple deployments
side by side trough branches.
Finally having a log of changes can be useful when things
break. 
Things *will* break. Such is the life of a tinkerer. 

I imagine some people still want to use btrfs,
so I'll leave the old guide in place.
However, I'll copy over parts which were good in here for convenience.
This updated guide still uses encrypted disks,
I've had no problems with this at all,
and I recommend disk encryption to all.

[^hardware]: I had uuid for my disks, but I swapped the disks so the boot bricked.

# Getting started
Get yourself a NixOS [live usb](https://nixos.org/download.html#download-nixos).
I use the minimal ISO, because the graphical ISO slows booting and gives no advantage
aside from being pretty.
Use
```
cat minimal-nixos.iso > /dev/sdX
```
where `X` is the usb drive found by `lsblk`.
`X` should be a letter, numbers indicate partitions,
which we don't want to cat upon because the ISO already
contains a partitioning scheme.

Boot into it on the target machine.
Become root with `sudo -i`.

# Internet
Next step is to setup WIFI, you can skip this if you're on Ethernet:

```bash
wpa_passphrase SSID PASS > /etc/wpa_supplicant.conf
systemctl restart wpa_supplicant
```

The first command creates a config for wpa_supplicant.
The reader must fill in SSID and PASS of his target wifi network.
The second command tells systemd to go restart wpa_supplicant and use the new config.

Ask google if you're online:
```
curl google.com
```
Should return a 301 Moved.
If it hangs or refuses the connection you likely have no internet.
There is no point proceeding until you have internet access.

# Partitioning
Now to setup the partitioning on the RIGHT device.
Choose carefully.
Use `lsblk` to figure out which device is RIGHT.
You'll know it's the WRONG device if you lose data after partitioning.
The RIGHT device will be called `$dev` hence forward.

There are no other partitioning tools than gdisk.
Only heretics believe there are.
Therefore we use gdisk:

```bash
gdisk $dev
```

## Gdisk cheat sheet

| Command | Effect                                                                 |
|---------|------------------------------------------------------------------------|
| `p`     | For printing, to see what's going on.                                  |
| `d`     | For deletion, you should start out with deleting everything on `$dev`. |
| `n`     | Is used for creating new partitions.                                   |
| `w`     | is used for writing once finished.                                     |

This table just describes the commands needed for the intended partitioning.

## Intended partitioning

| Number | type | size           |
|--------|------|----------------|
|      1 | ef00 | +500M          |
|      2 | 8200 | +$(SIZE_RAM+ a little)G |
|      3 | 8300 | (rest of disk) |


The first partition will be boot,
the second swap[^optional],
the third will be everything else.
We will encrypt everything else.
With type `ef00` we will use UEFI for booting.
Don't worry. nix will handle that, mostly. 
Done. Onwards!
[^optional]: This one is optional but allows hibernation.
           Which is very convenient for laptops.
           It can also make your system [more stable](https://askubuntu.com/questions/291378/do-we-still-need-swap-partitions-on-servers).

# Encryption
We use `cryptsetup` for encryption.
Make sure to select the right partition.
We do not want to encrypt the boot partition because then we can't boot.
So if you followed above instructions it will be either `3` or `p3`
(depending on device type).
We'll call it `3`.

```bash
cryptsetup luksFormat "$dev"3
cryptsetup open "$dev"3 nixenc
```

The first command does the actual formatting,
the second one opens up the formatted disk.
You'll need to provide the right password in both cases.
Choose one you can remember but is strong.
Once decrypted the disk will be mapped to `/dev/mapper/nixenc`,
note that we supplied that final part in the last command.

# Formatting filesystems
Partitioning is a distinct step from setting up filesystems.

```bash
mkfs.vfat -n boot "$dev"1
mkswap "$dev"2
swapon "$dev"2
mkfs.ext4 -L root /dev/mapper/nixenc
```
The boot partition will be `vfat` because [UEFI tells us to](https://wiki.archlinux.org/index.php/EFI_system_partition).
The everything else partition will be `ext4`.
Note that we point it at the mapped file,
if the `"$dev"3`device were to be used directly we'd remove the encryption.

# Mounting 
Here we mount all partitions.

```bash
mount /dev/mapper/nixenc /mnt/
mkdir /mnt/boot
mount "$dev"1 /mnt/boot
```

This makes them detectable by the nix config generation script.
Furthermore it allows the script to write the config on
the proper disk.

## Did I do everything right?
The second time I ran trough this post everything went 
quite quickly,
so I became skeptical.
To verify everything was sane I used the following commands:

```bash
mount | grep /mnt
ls /mnt
```

The first command is to check if the encrypted volume and boot is mounted at
the right paths.
The second one to verify the folders are created.

# Configure nix part 1, in the live environment
We use a script to generate an intial nix configuration,
which detects the hardware for us:
```bash
nixos-generate-config --root /mnt
```

This will give us a default config
which we can [customize](https://nixos.org/nixos/manual/index.html#sec-changing-config) now.
You can also pick up ideas from [my config](https://github.com/jappeace/linux-config/blob/work-machine/configuration.nix), although its a mess.
On a wireless laptop,
it's highly recommended to enable [wpa_supplicant](https://nixos.wiki/wiki/Wpa_supplicant):

```nix
networking.wireless.enable = true
```

Furthermore we're going to need the packages vim and git
to modify the config after booting into the system:
```
  environment = {
    systemPackages = [
        pkgs.git
        pkgs.vim
    ];
  };
```

Make sure to set the channel to the same as in your
tracked git configuration,
or alternatively be ready to deal with upgrades.
For example on a live ISO of `21.11` I downgraded to `21.05` with:
```
nix-channel --add https://nixos.org/channels/nixos-21.05 nixos
nix-channel --update
```
and then I changed the `configuration.nix` to:
```
 system = {
    stateVersion = "21.05";
  };
```
Dealing with channels is quite fragile,
so I have these commands copied as comments in my `configuration.nix`.

Once configuration is done we can install nix:

```bash
nixos-install
```

Don't worry, we can use `nixos-rebuild switch` to reconfigure nix whenever,
once we're booted into it.
Hopefully we boot successfully:

```bash
reboot
```
Booting is hard, don't worry if this goes wrong the first <s>10</s> 30 times.

You may need to enable UEFI in your BIOS.
It's up to the reader to figure that part out [^f-keys][^boot-issue].
Alternatively one could setup grub. Good luck with that.
You can't read the rest of this post until you've booted.
Go back if you haven't booted, you messed up.

[^f-keys]: press some f keys on boot, f11 or f2 maybe?
[^boot-issue]: An issue I encountered was that rather then selecting EUFI boot,
              the bios did a traditional boot on the disk.
              So EUFI was correctly installed,
              I just had to select the same disk but with the EUFI label from the bios.
              Yup, all kinds of stuff can go wrong with booting.

# Configure nix part 2, with git
Once we're booted into the installation,
the absolute paths for the symlinks are different.
For example root is no longer under `/mnt`, but under, well, root `/`.
So login as root[^display-manager] and clone your config project:

[^display-manager]: Once rebooted you may be stuck at the display manager.
                    Use `Alt+f1` to switch to another TTY and login as root,
                    then use `passwd your-user-name` to set an initial password for that user.
                    Use `Alt+f7` to go back to the display manager.

```bash
cd /
git clone https://github.com/jappeace/linux-config
chown jappie:users -R /linux-config
```
This puts the linux-config project on the `/linux-config` path.
Some old time linux/unix users may puke in their mouths
upon seeing the "standard" directories being ignored, but fuck them.
We mustn't forget to copy over the hardware generated
config into our git project:
```bash
cp /etc/nixos/hardware-configuration.nix /linux-config/hardware/branch-name.nix
```
Where branch name the name is for the git branch you'll use for this deployment.
More on that in the branches section.
Also we want to include this as a module in the `configuration.nix`
```nix
  imports = [ 
    ./hardware/branch-name.nix
  ];
```

The tracked configuration should be used by the system.
What I usually do is login as my own user,
and run the script [setup-nixos.sh](https://github.com/jappeace/linux-config/blob/work-machine/scripts/nixos-setup.sh):
```bash
exit
cd /linux-config/scripts/
./setup-nixos.sh
```

Which sets up the symlink from the git tracked configuration to
the standard location, in other words it does this:
```bash
ln -sf /linux-config/configuration.nix /etc/nixos/configuration.nix
```

This script also symlinks all relevant
dotfiles from linux config into the home folder.
You may want prefer [home manager](https://github.com/nix-community/home-manager)
to symlinking dotfiles.
But this works for me.

## Multiple machines desktops
Branches are ideal for managing multiple machines,
because it allows you to diverge oddities such as hardware specific configurations.
For example I also have this crummy display switch script which is only relevant
for the PC.
On the laptop it has to be slightly different (if used at all).

This branch setup also allows merging back configuration changes from other machines.
Which involves solving an ordinary git conflict.
I recommend the reader to use merges rather then rebases,
because that way git remembers how conflicts are resolved.

However I recently learned that a friend of mine handles this trough
a [module](https://github.com/erikbackman/nixos-config/blob/master/flake.nix#L54)
system.
He then runs that with `nixos-rebuild --flake .#machine-name`.
So rather then using branches he has an entrypoint per machine
which he calls out directly with the flake.
I find his setup interesting, and may move over to something like that in the future,
although since flakes are still experimental, I'll hold off.

Another alternative is multiple configuration.nix files
in a repository and let the symlink decide which should be used
for what machine.
This would avoid any merge conflicts,
although the versions between machines need to be similar.
I think I prefer merge conflicts.

# Secrets
I have three major secret sources.
1. ssh keys
2. gpg keys
3. The keepassxc database

The database in [synchting](https://syncthing.net/),
this gives me access to all services so that I can
simply generate new gpg and ssh keys per deployment.
Any other file manage service would do, but I like
syncthing because it's decentralized.

I go to https://localhost:8384 on some device that has the database,
and the target device on the same address.
I type over the device id into that screen to start syncing.
after syncing completes I have access to the keepasscx database.
Now I can generate new ssh keys and gpg keys,
and login to services to update those.
With that finished the installation is complete.

Aside from getting the keypass database up and running,
it's important to add your newly generated public key to
the services you manage.
For example this website is hosted on the [nixos multi monolith]({filename}/hetzner-nix-monolith.md).
It be prudent to add the ssh key via a machine that already 
has access to it.
Syncthing can also be used for this.
