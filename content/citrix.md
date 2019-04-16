TITLE: Citrix notes
DATE: 2019-04-12
CATEGORY: pain
Tags: pain, devops, virtualization, tools, linux
OPTIONS: toc:nil
Status: draft

A client had a citrix environment running.
To debug the deployment of a machine, I copied the ctrix enviroment locally
in a virtual box.

Note that this actually doesn't run any machines inside citrix, but it gets the GUI part operational.
It appears that in nixos you can just setup a local xen server which is the same as citrix
without GUI [here](https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/virtualisation/xen-dom0.nix)
Actually it may be easier to just run that on your bare metal platform and then
mount the GUI part inside a VM.

This post contains my notes.

# Xenserver
It's actually CentOS.
Also nixos makes smaller releases.

## Get xenserver
https://www.citrix.com
Don't bother googling this. It doesn't find the download.
You have to make an account I think.

Then go for the `citrix hypervisor` in the downloads area.

xenserver = `citrix hypervisor`

## Get client
The xenserver also contains the client.
No need to download any of the other crap (as I did).
xenserver is one of the many products hosted by citrix.
You don't care about any of them except xenserver.

# You can run xenserver in virtual box
   Lol, it's truly virtual.
   You need enough ram though, just create a swap file if you run into trouble.
   
   Make sure to [bridge](https://superuser.com/questions/227505/what-is-the-difference-between-nat-bridged-host-only-networking)
   it on a device like wifi.
   This allows us to access it from another VM.
   Also put promiscious mode on allow all. I think this is why I couldn't transfer the vhd.
   
   Install it, go into the root shell and type `ip addr` to get the ip.
   We need this for the client.

## Spel for creating an extra storage repostiroy:
   For some reason the xesnserver created a very small 8G repostiroy,
   I just added another disk with virtualbox (dynamicly sizing) and then
   ran this on the device:

xe sr-create name-label=<Storage ID> shared=false device-config:device=<Path of the Storage device> type=lvm content-type=user

   actually I attempted to partition it, but I think that command does that.
   (If not see my other post on gdisk).

# Use the windows client
	I couldn't get the linux client to work on nixos.
	Also you probably don't want to get into libvirt. (althoug they say they support remote).
   
   I ran this client in the edge iso provided by MS.
   Also make sure to bridge this image.
   No need to pay for anything if you're just doing one-off tasks.

## Click on a vmdk to import it
    I could find no way of doing this in the program itself

Also make sure to have a server selected before importing it,
otherwise you'll get a null reference error on the storage screen.
(It appearntly uses the selected one instead of the location from previous screen).
## Log for notices
  There is a log:
C:\Users\IEUser\AppData\Roaming\Citrix\XenCenter\logs

All these error messages are not displayed in the app
for some reason.
That information was of course incredibly usefull
so no idea why you'd hide that.


# Conclusion
  By the way, I recommend against using citrix for any orginazation.
  First of all seriously consider hosting in the cloud.
  It's so much easier to do this stuff on AWS or google cloud, where a bunch of things
  are already taken care of for you and you can automate the rest yourself.
  
  Secondly if you have to do it on-site for some reason.
  Use either virtualbox, qemu or docker.
  Actually I really like the idea of using containers directly on 'bare-metal'
  like joyent does. It's really efficient.
  
  If you don't have people that can do this for you, you know where to find me.
  I can teach.

# Nix image

The right image format appears to be a vhd.
You can make it like THIS.

# Nixos-rebuild

We can use nixos-rebuild to do inplace updates of the running vm.
This means you don't need to mess around with nixops for a single vm.

I ended up with this make file:
```shell
IP="192.168.0.39"
deploy: 
	NIXOS_CONFIG=$(shell pwd)"/image.nix" nixos-rebuild switch --target-host root@$(IP)
```

If you bridged your vm correctly you can find the ip with nmap:
```shell
   nmap -sn 192.168.0.0/24 
```
Your subdomain may be different but you can discover that with `ip addr`.
