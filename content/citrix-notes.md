TITLE: Citrix XenCenter 7.6 notes
DATE: 2019-04-18
CATEGORY: pain
Tags: pain, devops, virtualization, tools, linux
subreddit: Citrix

A client had a Citrix environment running.
To debug the deployment of a machine, I copied the Citrix environment locally
in VirtualBox.
Note that the server doesn't run any machines inside citrix,
but it does get the XenCenter operational.
It appears that in nixos you can just setup a local [xen server](https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/virtualisation/xen-dom0.nix).
The configuration described here will setup a Citrix XenServer that installs and runs,
it just can't run other VM's. It says it misses HVM.

This post contains my notes on getting XenCenter operational in VirtualBox.
It's also a guide for people familiar with Linux,
but not with Citrix.

# Citrix XenServer
It's actually CentOS with XenServer installed on it.
The VM shell will drop you into a ncurses menu which has an option to go to a bash shell.

The branding says it's a 'smaller' operating system, looks pretty complete to me though.
I think they mean that they have kernel modules that may allow the guest VM's
to use hardware directly.
To do this you have to modify the host as well as the guest OS.
They both have to understand the virtualization situation.

If you want to do this properly you should seriously consider [containers](https://www.youtube.com/watch?v=xXWaECk9XqM).
Even though virtual machines are [more secure](https://www.infoworld.com/article/3071679/linux-containers-vs-vms-a-security-comparison.html)
and virtual machines are more portable[^mix],
the trade off however is that virtual machines are really inneficient.
Giving them direct access to hardware won't
change the nature of OS kernels.
To hog all resources, which they have to do to manage them.
A container setup will simply have a single kernel, yet many operating systems (user spaces).

[^mix]: If you mix OS'es for some reason you're right in using virtual machines

## Get XenServer
Go to [citrix.com](https://www.citrix.com), you must get it from there.
Don't bother googling this.
Google doesn't find the download.
You *have* to make an account to download it.

Then go for the `citrix hypervisor` in the downloads area.
By the way:  `XenServer = citrix hypervisor`
I think the only thing they did to the XenServer is add branding.

## Get client
The XenServer also contains the client.
No need to download any of the other stuff.
XenServer is one of the many products hosted by citrix.

# You can run XenServer in virtual box
You need enough RAM though, just create a [swap file](https://linuxize.com/post/create-a-linux-swap-file/)
if you run into trouble.

Make sure to [bridge](https://superuser.com/questions/227505/what-is-the-difference-between-nat-bridged-host-only-networking)
it on a device like WIFI.
This allows one VM to access another VM.
Also put promiscuous mode on allow all.
I think this is why I couldn't transfer the VHD.

Install it, go into the root shell and type `ip addr` to get the ip.
We need this for the client.

## Creating an extra storage repository:
For some reason the XenServer created a very small 8G repository,
I just added another disk with VirtualBox and then
ran this on the XenServer vm shell:

```shell
xe sr-create name-label=<Storage ID> shared=false device-config:device=<Path of the Storage device> type=lvm content-type=user
```

Actually I attempted to partition it, but I think the command already does that.
If not see my other post where [gdisk]({filename}/nixos-encrypted-btrfs.md) is explained.

# Use the windows client
I couldn't get the citrix Linux client to work on nixos.
I attempted [libvirt](https://libvirt.org/) because it supports remote but it
seemed fairly complicated.
I ran the XenCenter client in the [edge iso](https://developer.microsoft.com/en-us/microsoft-edge/tools/vms/)
provided by MS.
This VM was bridged too.

Once inside the VM, 
click on a VMDK to import it into XenCenter.
I could find no way of doing this in the XenCenter program itself.
Also make sure to have a server selected before importing it,
otherwise you'll get a null reference error on the storage screen.
It apparently uses the selected one instead of the location from previous screen.

## Log for notices
If you ever get a "generic error", check the log:

```
    C:\Users\IEUser\AppData\Roaming\Citrix\XenCenter\logs
```

There is a ton of information in that.
Pasting the relevant part into google will get you more
progress than "generic error".

# VirtualBox VMDKs
Citrix XenServer does not appear to accept VirtualBox VMDKs.
I got VHD to work locally, but for some reason
it didn't work at the client.
If you want a NixOS VHD checkout [this post]({filename}/nixos-notes.md)
