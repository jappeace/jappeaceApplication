Title: Nixos on encrypted btrfs
Date: 2018-08-14 17:30
Category: tools
OPTIONS: toc:nil
Tags: nix, nixos, btrfs, tools, linux
subreddit: nixos

I decided to bite the bullet. Figure out how to get btrfs working on a luks
encrypted disk.
Turns out it wasn't as hard as I expected.
This post documents the journey and commands used.

First thing I did was make a move plan. 
I had the steps pretty clear in my mind,
but I didn't want to take risks.
Besides I would often need to use the phone to lookup commands so having a
document with all the neccisary links would come in handy.
This was recored in my [linux-config](https://github.com/jappeace/linux-config)
project around which the installation is centered,
considering I already had a workign config from a previous install on encrypted
ext4.
The plan can be seen below:

```md
# Nixos move plan

1. Backup partition
2. Format in btrfs with luks
3. Get ssh keys
4. clone this repo
5. get main keepass file.
6. setup syncthing
7. setup email

I think it's a bad idea to put everything in nix from the beginning
(I acrued to much over the years), 
but we can keep on adding custimizations over time.

## uniform style

Use lxappearance and qt5ct. Can only work trough magical buttons

## Resources

### btrfs

some thread where it maybe does work: https://github.com/NixOS/nixpkgs/issues/15786
script: https://gist.github.com/samdroid-apps/3723d30953af5e1d68d4ad5327e624c0

### emacs
https://github.com/NixOS/nixpkgs/blob/c836833c0125d31f5ec11e5121ba73f89ec4b9fa/pkgs/top-level/emacs-packages.nix

### Full disk encryption blog post
http://qfpl.io/posts/installing-nixos/
```

That plan is the content of this entire blog post.
However it's scattered over the various resources so for cohesion I'll make a
step by step instruciton here.
Backups are for losers. Do not backup your data. Losing is part of life.
I didn't contradict myself there.

# Getting started
Get yourself a nixos [live usb](https://nixos.org/nixos/download.html).
Boot into it.
First step is to setup WIFI:

```bash
wpa_passphrase SSID PASS > /etc/wpa_supplicant.conf
systemctl restart wpa_supplicant
```

# Partitioning
Now to setup the partitioning on the RIGHT device.
Use `lsblk` to figure out which.
The right device will be called `$dev` hence forward.
There are no other partitioning tools than gdisk.
Only heritics believe there are.
Therefore we use gdisk:

```bash
gdisk $dev
```

We use `p` for printing, to see what's going on.
`d` for deletion, you should start out with deleting everything on `$dev`.
`n` is used for creating new partitions.
`w` is used for writing once finished.
Create the following partition scheme:

| Number | type | size           |
|      1 | ef00 | +500M          |
|      2 | 8300 | (rest of disk) |


The first paritition will be boot, and the second everything else.
We will encrypt everything else.
With type `ef00` we will use eufi for booting.
Don't worry. nix will handle that. You just need to enable it in bios.
It's up to the reader to figure that part out.
(press some f keys on boot, f11 maybe?)
Done. Onwards!

# Encryption
We use `cryptsetup` for encryption.
Make sure to select the right partition. We do not want to encrypt the boot
paritition because then we can't boot.
So if you followed above instructions it will be either `2` or `p2`
(depending on device type).
We'll call it `2`.

```bash
cryptsetup luksFormat "$dev"2
cryptsetup open "$dev"2 nixos-enc
```

The first command does the actuall formating, the second one opens up the
formated disk.
You'll need to provide the right password in both cases.
Choose one you can remember but is strong. A random sentence will do.
Once decrypted the disk will be mapped to `/dev/mapper/nixos-enc`,
note that we supplied that final part.

# Formatting filesystems
We setup partitioning but setting up filesystems is a distinct step.

```bash
mkfs.vfat -n boot "$dev"1
mkfs.btfs -l root /dev/mapper/nixos-enc
```

The boot partition will be `vfat` because eufi tells us to.
The everything else partition will of course be `btrfs`.
Note that we point it at the mapped file, if the device were to be used directly
we'd remove the encryption.

# moutning and subvolumes
I'm just cargo culting this,
but wouldn't it be nice to have subvolumes on your btfs?
You'll get subvolumes like this!
Note that subvolumes are pretty nice for making backups,
because we can have a finer grained incremental backup.
They're also nice because we can put two operating systems on the same
partition, which is more space efficient.

```bash
mount -t btrfs /dev/mapper/nixos-enc /mnt/
btfs subvol create /mnt/nixos
umount /mnt
mount -t btrfs -o subvol=nixos /dev/mapper/nixos-enc /mnt
```

Here we are presumably doing precisely that.
First we create a nixos subvolume below the root subvolume,
eg the nixos operating system will not be installed in the root, but one node
below the root, allowing potentially more being installed.
I think this is a pretty good idea, even though I'm cargo culting,
and reverse explaining my cargo culting behavior.

```bash
btrfs subvol create /mnt/var
btrfs subvol create /mnt/home
btrfs subvol create /mnt/tmp
chmod 777 /mnt/tmp
```
Here we create subvolumes below the nixos subvolume.
This allows the btrfs backup tools to just backup the home directory.
Or just the root directory ignoring `var`, `tmp` and `home`.
That final step is an addition by me. 
Turns out that pulseaudio really doesn't work well if it can't write into /tmp,
many other programs probably neither.

```bash
mkdir /mnt/boot
mount "$dev"1 /mnt/boot
```

Here we mount the boot paritition.

# Configure nix
We can use hardware detection to figure out how to setup nix on this setup:

```bash
nixos-generate-config --root /mnt
```


Done.

Now the user needs to write his own nix config, or
[copy mine](https://github.com/jappeace/linux-config/blob/master/configuration.nix)
or cherry pick whatever they need (recommend).

Once configuration is done we can install nix:

```bash
nixos-install
```

Don't worry, we can use `nixos-rebuild switch` to reconfigure nix whenever once
we're booted into it.
Hopefully we boot succesfully:

```bash
reboot
```

You can't read the rest of this post untill you've booted,
go back if you haven't, you meshed up.

# final steps
Once rebooted you may be stuck at the display manager.
Use `Alt+f1` to switch to another tty and login as root,
then use `passwd your-user-name` to set an inital password.
Use `Alt+f7` to go back to the display manager.

I personally haven't moved all my configuration into nix yet
(it's a big project),
but I wrote a [script](https://github.com/jappeace/linux-config/blob/master/scripts/nixos-setup.sh)
that symlinks all dotfiles, and hardlinks
the `configuration.nix` to my linux-config project.

# Conclusion
btrfs encrypted on nixos.
