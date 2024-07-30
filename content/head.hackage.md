Title: Analyzing Haskell stability
Date: 2024-07-30 21:00
Category: tools
OPTIONS: toc:nil
Tags: haskell, programming, tools, tech-proposal
subreddit: haskell programming 

I joined the [haskell stability working group](https://github.com/haskellfoundation/stability) 
about a year ago.
I was freshly scarred from a GHC upgrade, so I thought it'd be 
nice to try improve the situation.
I think my first contribution was making an example for 
[GHC nightly builds](https://github.com/jappeace/haskell-nightly) on github actions.
The idea is that downstream maintainers can build
with GHC nightly to spot issues early.[^github-actions] 

[^github-actions]: At first I thought these mecahnisms were great, untill at some point upstream ghc started breaking for some reason and I learned that the availability was "best effort", causing me a bunch of issues so I turned it off. But I applied these "github actions" techinques I learned for various other repositories. Where I learned that for projects that have little activity, github likes to disable automations. Since 99% of my projects are like that, it essentially makes all of github actions kinda useless for me. I had pretty bad experience with github actions before all this, but this just made me even more estranged. It's extremly unreliable compared to the nix CI pipelines I'm used to.

Recently we've been curious about what Haskell ecosystem and GHC changes cause most breakage?
A suggestion was made to do a quantitative analysis of "head.hackage".
"head.hackage" is a repository of patches for hackage. 
GHC engineers use these to test out new GHC builds on a wide range of hackage packages 
without having to upstream[^upstream] a patch, which can take time.
We can analyze this repository of patches,
and put those patches into the categorization of [GHC Stability State of Play](https://docs.google.com/document/d/1sX_rXHx8Mj3Kae9GalR2BwZ5-xzl7UpnpMBwl4dqsWY/edit)
I volunteered for that effort.
Here is a preliminary result of the "head.hackage" patches required
to make around 485 packages[^estimate] build.

[^upstream]: Upstreaming is the process of sending a patch to the "maintainers" of an opensource projects. The maintainers will then make the patch "official", by merging it. In principle the process is simple, but in practice the burden of proof (especially for larger projects) is on the person who submitted the patch. They have to convince the maintainers the patch is usefull, which takes time in the form of comminucation.

[^estimate]: The estimate of 485 packages is done based on this [grafana dashboard](https://grafana.gitlab.haskell.org/d/7T7oEMlMz/head-hackage-performance?orgId=2&viewPanel=3&var-packages=All). The number changes over time. this measurement was made at 2024.07.28.

<iframe style="width:100%; height:20em;" src="https://docs.google.com/spreadsheets/d/e/2PACX-1vQR7N5UxVMFi8gGXkfowWceSMnxVAEtmRBjjdYxcEzyEJQh55ykfnz4hAR7xeJclnp5wiZh80HTG5f6/pubhtml?widget=true&amp;headers=false"></iframe>

From this we can see that in fact we're not doing to bad for stability in ghc directly.
Many of these patches are for older changes for example.
We're going to extend this work by labeling the exact cause of
breakage for each patch (many do overlap).
This will allow us to see more clearly which changes caused most
issues and help us prevent similar situations in the future.

Do you have any opinions on this document, 
do you feel stability could be better, or do you have a specific frustration which grinds your gears around stability,
Please reach out! 
We're always interested in hearing from the community and are in fact actively recruiting
members.
You don't need to join, you can also email or use the comment box below.
