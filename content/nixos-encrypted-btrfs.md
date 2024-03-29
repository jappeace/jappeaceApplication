Title: NixOS on encrypted btrfs
Date: 2018-08-19 13:02
Modified: 2022-01-30 13:18
Category: tools
OPTIONS: toc:nil
Tags: nix, nixos, btrfs, tools, linux, devops
subreddit: nixos linux

Nixos is heroin for tinkerers.
Paradise can be tinkered together and be freely shared among peers
because it's fully reproducible!
Jappie wanted more, he wanted a secure disk *and* a BTRFS.
There used to be no guides for this, now there is.

![Locked btrfs on nixos](/images/2018/locked_btrfs.svg)

The bullet was bitten, BTRFS was made to work on a LUKS encrypted disk.
This isn't hard, with care and precision.
To help a reader we document the journey towards BTRFS.
Commands compiled and included.

# Getting started
Get yourself a NixOS [live usb](https://nixos.org/download.html#download-nixos).
I use the minimal ISO, because the graphical ISO slows booting and gives no advantage
aside from being pretty.
Use
```
cat minimal-nixos.iso > /dev/sdX
```
where `X` is the usb drive found by `lsblk`.
`X` should be a letter, numbers indicate partitions, which we don't want to cat upon.
Boot into it on the target machine.

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

```html
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
           Note that 
           Swap files are [bad](https://wiki.archlinux.org/index.php/Btrfs#Swap_file)
           on BTRFS.

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
mkfs.btrfs -L root /dev/mapper/nixenc
```
The boot partition will be `vfat` because [UEFI tells us to](https://wiki.archlinux.org/index.php/EFI_system_partition).
The everything else partition will be `btrfs`,
because why are you following this guide if not?
Note that we point it at the mapped file,
if the `"$dev"3`device were to be used directly we'd remove the encryption.

# Moutning and subvolumes
Wouldn't it be nice to have subvolumes on your BTRFS?
This is not [cargo culted](https://en.wikipedia.org/wiki/Cargo_cult_programming)
at all.

```bash
mount -t btrfs /dev/mapper/nixenc /mnt/
btrfs subvol create /mnt/nixos
umount /mnt
mount -t btrfs -o subvol=nixos /dev/mapper/nixenc /mnt
```

First we create a nixos subvolume below the root subvolume,
eg the nixos operating system will not be installed in the root,
but one node below the root,
allowing potentially more operating systems to be installed on the same
partition.
Reverse explaining cargo culting behavior,
this maybe a good idea.

```bash
btrfs subvol create /mnt/home
```

Setting up a subvolume for `home` allows btrfs based backups.
I used too also make subvolumes for `tmp`[^cmod] and `var`,
but I don't see the merit in that.

[^cmod]: If you want a subvolume for /tmp, make sure to chmod it to 777
        Otherwise various applications get upset.
        Pulse audio for example doesn't work well if it can't write into `/tmp`.


```bash
mkdir /mnt/boot
mount "$dev"1 /mnt/boot
```

Here we mount the boot partition.
Just to make it detectable by the nix config generation script.

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
The second one to verify the folders are created, which are subvolumes.
The subvolume command creates a folder so if it exists we presume it worked.
But if you're really unsure you can use `btrfs subvol list /mnt/`.

# Configure nix
We can use hardware detection to figure out how to setup nix on this setup:

```bash
nixos-generate-config --root /mnt
```

Done.

Now the user needs to write his own [nix config](https://nixos.org/nixos/manual/index.html#sec-changing-config),
or [copy mine](https://github.com/jappeace/linux-config/blob/master/configuration.nix)
or cherry pick whatever they need (recommend).
On a wireless laptop,
it's highly recommended to enable [wpa_supplicant](https://nixos.wiki/wiki/Wpa_supplicant):

```nix
networking.wireless.enable = true
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

# Final steps
Once rebooted you may be stuck at the display manager.
Use `Alt+f1` to switch to another TTY and login as root,
then use `passwd your-user-name` to set an initial password.
Use `Alt+f7` to go back to the display manager.

I personally haven't moved all my configuration into nix yet
(it's a big project),
but I wrote a [script](https://github.com/jappeace/linux-config/blob/master/scripts/nixos-setup.sh)
that symlinks all dotfiles, and hardlinks
the `configuration.nix` to my linux-config project.
