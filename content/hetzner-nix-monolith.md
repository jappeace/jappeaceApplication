TITLE: Hetzner nix monolith.
DATE: 2017-10-02
CATEGORY: reflection
Tags: nix, programming, ops
OPTIONS: toc:nil
Status: draft

I've redone how my services are structured.
Instead of running each project on a separate VM,
they're now all running on a dedicated hetzner machine.
In other words, I'm renting an entire machine.
The services are separated trough multiprocessing.
The configruation is done with nix.
That is to say, any change on the machine is reflected
within nix files, in theory.
The big exceptions being the database and temporary files.

This post describes why I did this in the first place,
then we move on to how this was done in nix,
and finally there is a discussion segment because this
goes against the convention.

## Why bother with this?
Setting it up like this has a couple of advantages:

1. It's incredibly cheap
2. The setup is simple
3. No need for nixops.
4. Spinning out new services is really fast.

### 1
The price of a [hetzner](https://www.hetzner.com/dedicated-rootserver)
machine is 45 euro's per month.
This gives you 1 terrabyte of raid-1 disks,
12 threads and 64g of ram.
You [currently](https://aws.amazon.com/blogs/aws/new-t3-instances-burstable-cost-effective-performance/)
pay around 240 dollar per month for a t3.2xlarge instance.
That's halve the amount of ram and only 8 vCPU's (not dedicated threads)
and that doesn't include network cost or storage.
It's safe to say that hetzner is *cheap*.

### 2
The setup is simple because you 
don't have to do networking.
Everything is on the same machine,
which means you only have to do
inter process communication.
Furthermore, all services communicate to the same database
process,
which uses the databases' internal tenanting system
to ensure the data remains isolated.

The same goes for anything else services need,
if a service needs the filesystem,
just prefix some folder with their domain name, tenanting complete.
I don't have to deal with buckets or networking issues.
and if something doesn't work,
9 out of 10 times systemd will tell me exactly what's broken,
no need to dive into the AWS CLI.

### 3
Nixops doesn't have great UX,
it randomly just doesn't work,
sometimes because of aws issues,
or because of bugs in systemd or nix.

Furthermore the nixops developers decided to helpfully
change how the CLI works from the bash based counterparts.
For example SCP only works with `--from` and `--to` flags,
which arguably is better, more readable, but,
frustrating *years* of habits.

Not to speak of security issues when upgrading to 21.5
or the version 2 limbo which lasted for years.
I happily replace it if I can,
and in this case, I can!
Furthermore I love to cut out one of the variables.
I can cut out both nixops and the aws variable,
after all I don't need to manage aws. 

### 4
Finally the last advantage 
is that setting up a new service is *fast*.
No need to wait ages for an instance to boot,
starting a process is instant.
This is sort off the same as `3`, but in this case we're saying AWS 
is slow, rather then nixops having poor UX.

## The configuration 
I build this by relying on the [module system](https://nixos.wiki/wiki/Module).
The main entrypoint
is an ordinary nixos [configuration file](https://nixos.org/manual/nixos/stable/index.html#sec-configuration-file),
furthermore, all modules are also just ordinary configuration files.
So we got a very versatile one trick pony!
This is how the root configuration file looks
`/nix/hetzner/configuration.nix`:

```nix
{ config, pkgs, ... }:

{
  nixpkgs.config = import ../config.nix; # https://nixos.org/manual/nixpkgs/stable/#chap-packageconfig
  imports =
    [ 
      ./hardware-configuration.nix
      ../../videocut.org/nix/videocut.nix
      ../../massapp.org/nix/massapp.nix
      ../../raster.click/nixops/raster.nix
    ];
    ...
```
So that is has the same structure as the `configuration.nix` I use on [my laptop](https://github.com/jappeace/linux-config/blob/lenovo-amd/configuration.nix#L35).
Except I'm pulling in a bunch of modules aside from the hardware config.
The `imports` tell nixos to also include those other configuration files.
However before looking into that, I need to explain how to run this entrypoint.
I call this configuration from my `/makefile`:
```make
deploy:
	nix-shell nix/nixops-shell.nix --run "make deploy_"

ROOT_DIR:=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))

deploy_:
	NIXOS_CONFIG="$(ROOT_DIR)/nix/hetzner/configuration.nix" nixos-rebuild switch --target-host root@videocut.org --show-trace
```

`make deploy` will deploy if called form the root of the project.
This uses `nixos-rebuild switch`, just like on my laptop.
However I specify the target host to be one of the hosted service domains
which all point to the same hetzner machine.

So how does the machine know which HTTP request send to what service?
Nginx can do that! `/nix/hetzner/configuration.nix`:

```nix
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
Virtual hosts allow us to specify configurations per domain.
For example the [massapp.org](https://massapp.org/)
host looks like this `/massapp.org/nix/vhost.nix`:

```
  lib = import ./lib.nix;
  sslBools = {
    forceSSL = true;
    enableACME = true;
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
which strips off `www` from `www.masssapp.org` into `massapp.org`.
For some reason people still yearn to type `www`.
Finally in `C` we strip of the https,
which the proxy will remake into an `http` connection.
This means our application doesn't have to deal with certificates or SSL.
Which is safe because this traffic is internal. 

So every service or application gets a unique port.
We have to bind a program to that port.
within the massapp module, another `configuration.nix` file, I register
the systemd service which runs the main app `/massapp.org/nix/massapp.nix`:
```
{ config, pkgs, ... }:
let
    massapp = pkgs.callPackage ../webservice/default.nix { }; # A
in {
      ...
      systemd.services.massapp =
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
can even pass CLI arguments like this just by modifying the ExecStart.

## Database integration
It's not particularly hard setting up the database process
`/nix/hetzner/configuration.nix`:

```nix
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
      CREATE USER massapp_prod WITH PASSWORD 'someinitialpassword;
      CREATE DATABASE massapp_prod;
      ALTER USER massapp_prod WITH SUPERUSER;
      '';
   ...
   }
```

This `C` initial script is only run when the database
is started for the first time,
after that you need to manage it by hand.
However I still add these users to script to keep track
of them if I ever need to abandon this system.
Like this at least the users will exist on the new machine.
I give every service user superuser access in `C`.
This isn't the best for security, but it's really convenient.
Besides if a hacker manages to get a shell for the system
or a shell for the database it's already to late for me anyway.
I'd just scrap this system and start over elsewhere.

In `B` I set some additional logging options,
which are just convenient when things break.
With statment logging I can still see what happened
in the database if the application didn't emit enough information.
logging all statements [slows down](https://dba.stackexchange.com/questions/30144/performance-impact-of-setting-postgresql-to-log-all-statements) the database.
However, my services are hardly taxed at the moment,
they are taxed but no where near justifying not logging.
In `A` I enable local authentication always without password.
This means I can log in from within the machine with a root shell
with no fuzz.
Connections from outside the machine are refused.

## Discussion
This setup goes against [advice](https://www.reddit.com/r/sysadmin/comments/92qhuu/should_i_put_multiple_services_on_a_single_vm_or/)
from sysadmins, where they recommend
you split up everything across VM's as much as possible.
Maybe the difference here is that this deployment is nix based,
which allows me to manage the complexity of the monolith
much more efficiently.
Any change is reflected in files after all,
instead of having a bag of ubuntu state where you're
always unsure about what happened when.
Furthermore, I'm really small time compared to an enterprise solution.
For example the most users a service ever had was about 60.

I feel this approach falls inline with the [monlith first](https://martinfowler.com/bliki/MonolithFirst.html)
approach,
but rather then a single monolith I can deploy many
completly unrelated projects.
Why isn't this a micro serivce architecture then?
There are several small services running on a single machine.
Whereas micro services are a single project seperated
for some reason or another,
the projects on this service are completly unrelated.

For example, it hosts:
+ this blog.
+ [raster.click](https://raster.click/) a rostering system for restaurants.
+ [videocut.org](https://videocut.org/) an automated video editor.
+ [massapp.org](https://massapp.org/) a program for contacting customer lists trough whatsapp.

In the past I've used it as well to quickly drag a new website out of
the ground to impress potential clients.
Nothing is more impressive then have a functioning website,
*the next day*.
What do I have to do?
I just have to get the domain, for example `newdomain.com`,
copy over the `massapp.org` folder into `newdomain.com`,
hook it into the main config`/nix/hetzner/configuration.nix`,
setup the database, deploy.
and that's it.
That's two hours of work, maybe three if something goes wrong.

If you're having doubts like me about nixops and want to replace it,
you may run into secret management issues.
Fortunately there is an alternative project to do that,
[agenix](https://github.com/ryantm/agenix) could
be used for secret management.
Although I'm a bit skeptical over [age](https://github.com/FiloSottile/age),
it seems to new to trust.
You should do your diligence before using that in production.
