Title: Nixos managed with git
Date: 2022-01-30 16:20
Category: tools
OPTIONS: toc:nil
Tags: nix, nixos, ext4, tools, linux, devops
Status: draft

A few years ago I wrote a post on installing
NixOS [on encrypted btrfs](./nixos-encrypted-btrfs.md).
I recently went trough that guide to install
NixOS once more.
The guide is good, but for me it has some issues:

1. It uses btrfs which I no longer use, due to performance concerns
2. No mentioning of git, which composes really well with nix and nixos.
3. It doesn't explain how to deal with secrets.

I'll address these concerns here,
since I *just* fucked up an install due to git usage [^hardware].
I imagine some people still want to try using btrfs for whatever
reason, so I'll leave the old guide in place.
But I'll at verbatim copy old pieces which were good.

[^hardware]: I had uuid for my disks, but I swapped the disks so the boot bricked.


# Getting started
Get yourself a NixOS [live usb](https://nixos.org/download.html#download-nixos).
I use the minimal ISO, because the graphical ISO slows booting and gives no advantage
aside from being pretty.
You can `cat minimal-nixos.iso > /dev/sdX`, where `X` is the usb drive found by `lsblk`.
`X` should be a letter, numbers indicate partitions, which we don't want to cat upon.

Boot into it on the target machine.
Become root with `sudo -i`.

# Networking
Next step is to setup WIFI, you can skip this if you're on Ethernet:

```bash
wpa_passphrase SSID PASS > /etc/wpa_supplicant.conf
systemctl restart wpa_supplicant
```

The first command creates a config for wpa_supplicant.
The reader must fill in SSID and PASS of his target wifi network.
The second command tells systemd to go restart wpa_supplicant and use the new config.

Ask google if you're online: `curl google.com` should return a 301 redirect:
```
<HTML><HEAD><meta http-equiv="content-type" content="text/html;charset=utf-8">
<TITLE>301 Moved</TITLE></HEAD><BODY>
<H1>301 Moved</H1>
The document has moved
<A HREF="http://www.google.com/">here</A>.
</BODY></HTML>
```

There is no point proceeding until you have networking.

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
Wouldn't it be nice to have subvolumes on your BTRFS?

```bash
mount /dev/mapper/nixenc /mnt/
mkdir /mnt/boot
mount "$dev"1 /mnt/boot
```

Here we mount the boot partition.
Just to make it detectable by the nix config generation script.

## Did I do everything right?
Doing this a second time my speed made me skeptical,
to verify everything was sane I used the following commands.

```bash
mount | grep /mnt
ls /mnt
```

The first command is to check if the encrypted volume and boot is mounted at
the right paths.
The second one to verify the folders are created, which are subvolumes.
The subvolume command creates a folder so if it exists we presume it worked.

# Configure nix 1
We can use hardware detection to figure out how to setup nix on this setup:

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

Furthermore we're going to need the packages vim and git:
```
  environment = {
    systemPackages = [
        pkgs.git
        pkgs.vim
    ];
  };
```

Once configuration is done we can install nix:

```bash
nixos-install
```

Don't worry, we can use `nixos-rebuild switch` to reconfigure nix whenever once
we're booted into it.
Hopefully we boot successfully:

```bash
reboot
```
Booting is hard, don't worry if this goes wrong the first <s>10</s> 30 times.

You may need to enable UEFI in your BIOS.
It's up to the reader to figure that part out.
(press some f keys on boot, f11 maybe?).
Alternatively one could setup grub. Good luck with that.

You can't read the rest of this post until you've booted,
go back if you haven't, you messed up.

# First login
Once rebooted you may be stuck at the display manager.
Use `Alt+f1` to switch to another TTY and login as root,
then use `passwd your-user-name` to set an initial password.
Use `Alt+f7` to go back to the display manager.

I personally haven't moved all my configuration into nix yet
(it's a big project),
but I wrote a [script](https://github.com/jappeace/linux-config/blob/master/scripts/nixos-setup.sh)
that symlinks all dotfiles, and hardlinks
the `configuration.nix` to my linux-config project.

# Configure nix II, with git
Once we're booted into the instalation,
the absolute paths for the symlinks are different.
For example root is no longer under `/mnt`, but under, well, root `/`.

So login as root and clone your config project:
```
cd /
git clone https://github.com/jappeace/linux-config
```

This puts the linux-config project on the `/linux-config` path.
Some old time linux/unix users may puke in their mouths
upon seeing the "standard" directories being ignored, but fuck them.

Now what I usually do is login as my own user, and run the 
script [setup-nixos.sh](https://github.com/jappeace/linux-config/blob/work-machine/scripts/nixos-setup.sh).
which sets up the symlink from the git tracked project to
the standard location:
```
ln -sf /linux-config/configuration.nix /etc/nixos/configuration.nix
```
Now we mustn't forget to copy over the hardware generated
config into our git project:
```
cp /etc/nixos/hardware-configuration.nix /linux-config/hardware/branch-name.nix
```
Where branch name the name is for the git branch you'll use for this deployment.
More on that in the branches section.

The other aspect of this script is that it symlinks all relevant
dotfiles from the home folder into the linux-config.
This is pretty much a not invented here home manager.
I suppose if you've gone to the effort of setting up
home manager, that solution is more thorough,
but this was very low effort on my part
say maybe a couple of hours,
and has worked for years.
So I don't see the appeal at all of getting into home manager.

## Branches
Branches are ideal for managing multiple machines,
because it allows you to diverge oddities such as hardware specific configurations.
For example I also have this crummy display switch script which is only relevant
for the PC.
On the laptop it has to be slightly different.

This setup also allows you to merge back configuration changes from other machines.
To merge back you'll just have to solve an ordinary git conflict.
I recommend the reader to use merges rather then rebases,
because that way git remembers how conflicts are resolved.

# Secrets

I have three major secret sources.
1. ssh keys
2. gpg keys
3. The keepassxc database

I put the database in [synchting](https://syncthing.net/),
this gives me access to all services so that I can
simply generate new gpg and ssh keys per deployment.
Any other file manage service would do, but I like
syncthing because it's decentralized.

I go to https://localhost:8384 on some device that has the databse,
and the target device on the same address.
I simply type over the device id.

After syncing is complete we can generate a new ssh key,
and put it into github for example.
Then we can update the remote of linux-config.

Once this is complete I have something which I consider a functioning
deployment.
