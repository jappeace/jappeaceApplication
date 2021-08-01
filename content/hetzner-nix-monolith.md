TITLE: The Nix mutli-monolith machine (NMMM).
DATE: 2021-07-31 15:05
CATEGORY: reflection
Tags: nix, programming, ops
OPTIONS: toc:nil
Modified: 2021-08-01 15:09

![a NMMM computer](images/2021/computer-nixos-monolith.png)

I redid how my services are structured.
Instead of running each project on a separate VM,
they're now all running on a dedicated Hetzner machine.
This is what I call the nix multi monolith machine
(hence forth called NMMM,
pronounced like tasting something delicious prefixed with an N).
There are some really big advantages to the NMMM approach.

## Why use a NMMM?
Setting up NMMM has a couple of advantages:

+ the NMMM is cheap [^cheap]
+ the NMMM is simple [^simple]
+ the NMMM doesn't need nixops while benefiting from nix [^no-nixops]
+ the NMMM spins out new services fast [^fast]

Obviously there are also some disadvantages
to this approach,
which get answered in the [discussion section](#discussion).
But for a typical startup situation,
where money is tight and there is no
product market fit yet,
and time to market is important,
I think the NMMM is the best.

[^cheap]:   The price of a [Hetzner](https://www.hetzner.com/dedicated-rootserver)
        machine is `€ 45` euro's per month.
        This gives you 1 terabyte of raid-1 disks,
        12 threads and 64g of ram.
        You [currently](https://aws.amazon.com/blogs/aws/new-t3-instances-burstable-cost-effective-performance/)
        pay around `$ 240` dollar per month for a t3.2xlarge instance.
        That's halve the amount of ram and only 8 vCPU's (not dedicated threads)
        and that doesn't include network cost or storage.
        It's safe to say that Hetzner is *cheap*.
        <p>Of course on AWS you can get a much smaller machine for a cheaper price per month,
        which may sound good initially,
        but as soon as you need larger or more machines,
        a dedicated machine become much more cost effective.
        And you'll need a larger machine eventually.
        If your idea fails you probably want to keep it online for your resume,
        which would make it one of the monoliths,
        or it succeeds and you need a bigger machine to deal with memory volatility.
        I happened to need a large machine to do video editing for [videocut.org](https://videocut.org/).
        </p>

[^simple]: The setup is simple because you 
                don't have to do networking.
                Everything is on the same machine,
                which means you only have to do
                inter process communication.
                Furthermore, all services communicate to the same database
                process,
                which uses the databases' internal tenanting system
                to ensure the data remains isolated.
                <p>The same goes for anything else services need,
                if a service needs the filesystem,
                just prefix some folder with their domain name.
                I don't have to deal with buckets or networking issues.
                and if something doesn't work,
                9 out of 10 times systemd will tell me exactly what's broken,
                no need to dive into the AWS CLI.</p>

[^no-nixops]: For me nixops causes a lot of needles friction
              which I like to avoid.
              For example,
              the nixops developers decided to 
              change how the CLI works from the bash based counterparts.
              `nixops scp` only works with `--from` and `--to` flags.
              It refuses to accept the ordinary scp syntax, for no reason.
              <p>There are countless other examples of this kind of friction.
              I just get frustrated by writing about it, so I won't.
              However the more important thing this does
              is to cut out one of the variables during deployment.
              I can cut out both Nixops and the AWS variables,
              after all I don't need to manage AWS, so why would I use nixops? </p>

[^fast]: Finally the last advantage 
         is that setting up a new service is *fast*.
         No need to wait ages for an instance to boot,
         starting a process is nearly instant after all.
         This is sort off the same as `3`, but in this case we're saying AWS 
         is slow, rather then nixops having poor UX.

## What is NMMM
The core of the Nix mutli-monolith machine (NMMM) is
a dedicated machine.
A dedicated machine means no virtualization,
in other word it's bear metal.

The second major part of this configuration is nix.
This means that every piece of software is described in nix files.
Just like the configuration of this software.
More on that in the [nix config](#nix-config) section.

The third major part is multi-monoliths.
Meaning that you can have multiple,
unrelated services running on the same machine.
This is something different from micro services.
[Microservices](https://microservices.io/) communicate with each other at some point
to provide a consistent frontend.
The [link ](https://microservices.io/) calls this 'loosly coupled',
or as I liked to call it: They chat with each other.
Monoliths on the other hands should *not* communicate
with each other and are isolated.
They have independent frontends and backends.
In other words, no chatting between monoliths.
They just stand there [silent and ominous](https://www.youtube.com/watch?v=cHWs3c3YNs4).
I feel this approach falls inline with the [monlith first](https://martinfowler.com/bliki/MonolithFirst.html)
approach,
but rather then a single monolith I can deploy many
completely unrelated projects.

## <a id="nix-config"></a> Nix config

I build this by relying on the [module system](https://nixos.wiki/wiki/Module).
The main entrypoint
is an ordinary nixos [configuration file](https://nixos.org/manual/nixos/stable/index.html#sec-configuration-file),
other modules in general are also shaped like this even
though the entry point depends on them.
So we got a very versatile one trick pony!

Assuming `/` is the root of the project,
the root configuration looks like this:

```nix
# /nix/hetzner/configuration.nix
{ config, pkgs, ... }:

{
  imports =
    [ 
      ./hardware-configuration.nix
      ../../videocut.org/nix/videocut.nix
      ../../massapp.org/nix/massapp.nix
      ../../raster.click/nixops/raster.nix
    ];
    ...
```

That has the same structure as the `configuration.nix` I use on
[my laptop](https://github.com/jappeace/linux-config/blob/lenovo-amd/configuration.nix#L35).
The difference this example has with my laptop is that
I'm pulling in a bunch of additional modules aside from the hardware config
trough the `imports` mechanism.
`imports` tells nixos to also include those other configuration files.
However before looking into those specific files, I need to explain how to run this entrypoint.
I call this configuration from my `/makefile`:
```make
deploy:
	nix-shell nix/nixpkgs-shell.nix --run "make deploy_"

ROOT_DIR:=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))

deploy_:
	NIXOS_CONFIG="$(ROOT_DIR)/nix/hetzner/configuration.nix" nixos-rebuild switch --target-host root@videocut.org --show-trace
```

`make deploy` will deploy if called form the root of the project `/`.
It runs `make deploy_` from a shell that sets the `NIX_PATH`
to a [pinned nixpkgs](https://jappieklooster.nl/pinning-nixops-builds.html).
This uses `nixos-rebuild switch`, just like on my laptop.
However I specify the target host to be one of the
domains hosted on the Hetzner machine.
All domains domains lead to the Hetzner machine.

But how does the machine decide which HTTP request goes to what service?
After all they all arrive on the same machine now,
so something has to make a decision on this.
[Nginx](https://www.nginx.com/) can do that!
this is a bit later in the entry point file:

```nix
  # /nix/hetzner/configuration.nix
  ...
  services.nginx = {
    virtualHosts =  pkgs.lib.foldr (x: prev: pkgs.lib.recursiveUpdate (import x) prev) {}
      [../../videocut.org/nix/vhost.nix
       ../../massapp.org/nix/vhost.nix
       ../../raster.click/nixops/vhost.nix
       ../../blog/vhost.nix
      ]
  }
  ...
```
We let the individual services decide how to configure the virtual
hosts.
Virtual hosts allow us to specify configurations per [domain name](http://nginx.org/en/docs/http/request_processing.html).
For example the [massapp.org](https://massapp.org/)
host looks like this:

```
  # /massapp.org/nix/vhost.nix
  lib = import ./lib.nix;
  sslBools = {
    forceSSL = true;
    enableACME = true; # E
  };
  base = locations:
    {
      inherit locations;
    } // sslBools; # C
  proxy = base {
    "/".proxyPass = "http://127.0.0.1:${
        toString lib.massapp_port # D
      }/"; 
  };
  redirect = { globalRedirect = "${lib.massappDomain}"; } // sslBools;
in {
  "www.${lib.massappDomain}" = redirect; # B
  "${lib.massappDomain}" = proxy; # A
```
The main thing we're saying at `A` is that the [massapp.org](https://massapp.org/)
domain should point to the port defined at `D`.
Furthermore in `B` we're redirecting all `www` traffic to `A`.
Which strips off `www` from `www.masssapp.org` resulting into `massapp.org`.
For some reason people still yearn to type `www`.
With this redirect we trash the peoples' pointless dreams and desires.
Finally in `C` we strip of the HTTPS,
which the proxy will remake into an HTTP connection.
Traffic at this point is internal, so [SSL](http://www.steves-internet-guide.com/ssl-certificates-explained/)
has served it's purpose
and we can safely strip it.
In practice this means our application doesn't have to deal with certificates or SSL.
This leverages nixos [built-in](https://nixos.wiki/wiki/Nginx) [let's encrypt](https://letsencrypt.org/)
support without even thinking about it in `E` [^breaks-often].

[^breaks-often]: It's built in but this used to break quite a lot,
                 this is because it's enterly reliant on the graces of let's encrypt service.
                 Which may refuse you to give a certificate for all [kinds of reasons](https://letsencrypt.org/docs/rate-limits/).

Since we just bound all incoming domains to a unique port,
we have to bind a program to that port as well.
This program is our main application code, or the monolith.
For example let's look at the massapp module.
Herein I register
a systemd service which runs the main massapp executable.
This is another file like `configuration.nix`
(just like my laptop! The one trick pony):

```nix
# /massapp.org/nix/massapp.nix
{ config, pkgs, ... }:
let
    massapp = pkgs.callPackage ../webservice/default.nix { }; # A
in {
      ...
      systemd.services.massapp = # D
        {
        description = "Massapp webservice";
        serviceConfig = {
            Type = "simple";
            ExecStart = "${ massapp }/bin/massapp"; # B
        };
        wantedBy = [ "multi-user.target" ];
        requires = [ "postgresql.service" ];
        environment = {
            PORT = "${toString lib.massapp_port}"; # C
            ...
        };
    };
}
```
Within `A` I load the main binary of the service.
We tell systemd about that program at `B`.
Finally we make the program aware of the correct port in `C`
by setting the environment variable `PORT`.
[Yesod](https://www.yesodweb.com/) has a configuration built in for that by default,
and massapp is a [Yesod](https://www.yesodweb.com/) application.
This would work for any other application, you
can even pass CLI arguments like this just by modifying the `ExecStart`.
At `D` we give this systemd unit the name massapp, 
by doing this std out is logged in journalctl and tagged with the unit name.
Nixos-rebuild will now also know if a service failed trough exit codes.
For example if the service can't find the database.
Which we discuss how to setup in the [database](#database) section.

## <a id="database"></a> Database integration
This is the database configuration, again in the entrypoint
`configuration.nix` file:

```nix
  # /nix/hetzner/configuration.nix
  ...
  services.postgresql = {
    enable = true;
    # A
    authentication = pkgs.lib.mkOverride 10 ''
      local all all trust
      host all all ::1/128 trust
      host all all 0.0.0.0/0 md5
      host all all ::/0       md5
    '';
    settings = {  # B
      log_connections = "yes";
      log_statement = "all";
      log_disconnections = "yes";
    };

    # C
    initialScript = pkgs.writeText "backend-initScript" ''
      CREATE USER massapp_prod WITH PASSWORD 'someinitialpassword';
      CREATE DATABASE massapp_prod;
      ALTER USER massapp_prod WITH SUPERUSER;
      '';
   ...
   }
```

The initial script`C` is only run when the database
is started for the first time,
after that you need to manage users by hand.
However I still add these users to script to keep track
of them if I ever need to abandon this system.
Like this at least the users will exist on the new machine.
I give every monoliths' user superuser access in `C`.
This isn't the best for security, but it's really convenient.
Besides if a hacker manages to get a shell for the system
or a shell for the database it's already to late for me anyway.
I'd just scrap this system and start over elsewhere.

In `B` I set some additional logging options,
which are just convenient when things break[^it-breaks-always].
With statement logging I can still see what happened
in the database if the application didn't emit enough information.
logging all statements [slows down](https://dba.stackexchange.com/questions/30144/performance-impact-of-setting-postgresql-to-log-all-statements) the database.
However, my services are hardly taxed at the moment,
they are taxed but no where near justifying not logging.

Localhost connections can authenticate without password in `A`.
Outside connections are ignored, they're not even asked for a password.

[^it-breaks-always]: When do things break you ask? Well of course all software is broken so we always need to enable this.

## <a id="discussion"></a> Discussion
This setup goes against [advice](https://www.reddit.com/r/sysadmin/comments/92qhuu/should_i_put_multiple_services_on_a_single_vm_or/)
from sysadmins, where they recommend
you split up everything across VM's as much as possible.
I reject [their hypothesis](https://www.youtube.com/watch?v=nyErR1FS1_0). 
I mean they argue for splitting services,
but the tool used for splitting shouldn't be dogmatic
and always result in choosing a VM.
Using multiprocessing,
and tenanting for specific software packages is good enough.
[myron semacks](https://www.reddit.com/r/sysadmin/comments/92qhuu/should_i_put_multiple_services_on_a_single_vm_or/e37r7pm/)
at least gives some arguments on why he chooses to use a VM.
However his reasons for choosing a VM's
revolve around windows related oddities.
For example:

> Think about what happens when you need to upgrade the OS, which typically means you make a new VM to replace the old one.

We can seamlessly upgrade nixos pretty much always.
I've had configuration files change which caused errors when upgrading nixos stable versions,
but these errors need to be solved before deploying.
The systems I use with nixos themselves have always been stable.
Or: 

> Think about what happens when there is a problem and the VM is down. (Bad Windows update, you fat fingered a network setting, etc)

How would I even fat finger a network setting?
I have to modify a file, commit the file and deploy to do this.
At this point you can't even call it fat fingering.
And even if you somehow screw up a network setting deliberately,
you can reboot from an older generation and the problem is gone.
The difference here is that this deployment is nix based,
which allows me to manage the complexity of the monolith
much more efficiently and reliably.

NMMM can also be used to impress potential clients
by quickly creating new websites.
What are the steps?

1. Get the new domain, for example `newdomain.com`,
2. copy over the `massapp.org` folder into `newdomain.com`,
3. hook it into the main config`/nix/hetzner/configuration.nix`,
4. setup the database,
5. redo branding 
6. deploy.

That's it.
That's two hours of work, maybe three if something goes wrong.
Nothing is more impressive then have a functioning website,
*the next day*.
I guess aside from startups,
consultancies should also look into this approach.

On several occasions I've mentioned that `configuration.nix` is
just like on my laptop.
Originally I even copied pasted the Postgres
configuration from my laptop to this machine.
It just works.
This is one of the big benefits you get out of nix,
it's called the [copy-paste monad](https://www.youtube.com/watch?v=OyfBQmvr2Hc&t=2305s).

If you're having doubts about nixops and want to replace it,
you may run into secret management issues.
Fortunately there is an alternative project for secret management called
[agenix](https://github.com/ryantm/agenix).
Although I'm a bit skeptical over [age](https://github.com/FiloSottile/age),
it seems to new to trust.
You should do your diligence before using that in production.

## Conclusion

I described this NMMM setup that's working really well for me.
Furthermore I think I gave compelling reasons for
others to try this out.
Perhaps I liberated some people from their
[virtual insanity](https://www.youtube.com/watch?v=4JkIs37a2JE).
Let me know if I've inspired you to change your course of action,
or you've some compelling reasons not to use this approach.
Because how to structure services is a large decision,
I find this all fascinating.


## Further reading

+ [The NixOS configuration file explained](https://nixos.org/manual/nixos/stable/index.html#sec-configuration-file)
+ [NixOS module system](https://nixos.wiki/wiki/Module)
+ [Setting up a Hetzner machine with NixOS script](https://gist.github.com/nh2/ebc27311731f53ee623ae781ca25103f)
+ General nix advice:
    + [nix.dev](https://nix.dev/)
    + [nix 'pills'](https://nixos.org/guides/nix-pills/)
