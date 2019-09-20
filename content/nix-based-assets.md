TITLE: Nix based asset management
DATE: 2019-04-18
CATEGORY: technique
Tags: devops, tools, linux, nix, nixos, nixops, nginx
subreddit: nixos

[Nix](https://nixos.org/) can be used for efficient [reproducible](https://en.wikipedia.org/wiki/Reproducible_builds) browser caching.
With this technique one can trace back every dependency that
was used for generating the asset.
It's also fairly efficient with keeping the browser cache valid,
and combines well with [server side rendering]({filename}/server-side-rendering-reflex.md).

There are other solutions such as
[webpack](https://webpack.js.org/guides/asset-management/)
for javascript and 
[webassets](https://github.com/miracle2k/webassets) for python.
However they just make sha's of the end result.
Nix does that for every fairly agressevly[^dependency] on the way,
and in a fairly elegant manner as well.

[^dependency]: which means if something as low level as the 
	compiler for the minifier changes, the output sha of the end 
	product also changes.
	This will evict the cache on the client side.
	But also make that result tractable, we know where it came from.

Today we'll discuss a nix based solution,
which pushes the nix store to the browser.
We take the hash of the store path
and put this in the file name of the asset.
This results in a very reliable and efficient
cache eviction scheme.
If any build input changed, source code, compilers etc,
the name also changes.

Nix is pretty much langauge independent. 
In fact I'm using a haskell program that gets
compiled to javascript as an  example.
So this obviously will also work for javascript.
