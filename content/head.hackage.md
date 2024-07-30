Title: Analyzing Haskell stability
Date: 2024-07-30 21:00
Category: tools
OPTIONS: toc:nil
Tags: Haskell, programming, tools, tech-proposal
subreddit: Haskell programming 

I joined the [Haskell Stability Working Group](https://github.com/haskellfoundation/stability) 
about a year ago.
I was freshly scarred from a GHC upgrade, 
so I decided to try improving the situation.
I think my first contribution was making an example for 
[GHC nightly builds](https://github.com/jappeace/haskell-nightly) on GitHub Actions.
This allows downstream[^downstream] maintainers to build
GHC nightly to spot issues early.[^github-actions] 

[^downstream]: Downstream in this case means people who use GHC and are dependent on it.

[^github-actions]: At first, I thought these mechanisms were great. However, at some point upstream GHC started breaking for some reason and I learned that the availability was 'best effort', causing me a bunch of issues. So I turned it off. But I applied these "GitHub Actions" techniques I learned for various other repositories. I learned that for projects that have little activity, github likes to disable automations. Since 99% of my projects are like that, it essentially makes all of GitHub Actions kinda useless for me. I had a pretty bad experience with GitHub Actions before all this, and this made me even more estranged. It's unreliable compared to the Nix CI pipelines I'm used to.

Recently, we've been curious about what Haskell ecosystem and GHC changes cause the most breakage.
A suggestion was made to do a quantitative analysis of "head.hackage".
"head.hackage" is a repository of patches for Hackage. 
GHC engineers use these to test out new GHC builds on a wide range of Hackage packages 
without having to upstream[^upstream] a patch, which can take time.
Instead, they can put the patch in "head.hackage" and immediately test it on a wide range of packages.
We can analyze this repository of patches and
categorize them according to the  [GHC Stability State of Play](https://docs.google.com/document/d/1sX_rXHx8Mj3Kae9GalR2BwZ5-xzl7UpnpMBwl4dqsWY/edit)
I volunteered for that effort.
Here is a preliminary result of the "head.hackage" patches required
to make around 485 packages[^estimate] build.

[^upstream]: Upstreaming is the process of sending a patch to the "maintainers" of an open-source project. The maintainers will then make the patch 'official' by merging it. In principle, the process is simple, but in practice, the burden of proof (especially for larger projects) is on the person who submitted the patch. They have to convince the maintainers that the patch is useful, which takes time in the form of communication.

[^estimate]: The estimate of 485 packages is done based on this [Grafana dashboard](https://grafana.gitlab.haskell.org/d/7T7oEMlMz/head-hackage-performance?orgId=2&viewPanel=3&var-packages=All). The number changes over time. this measurement was made on 2024-07-28.

<iframe style="width:100%; height:20em;" src="https://docs.google.com/spreadsheets/d/e/2PACX-1vQR7N5UxVMFi8gGXkfowWceSMnxVAEtmRBjjdYxcEzyEJQh55ykfnz4hAR7xeJclnp5wiZh80HTG5f6/pubhtml?widget=true&amp;headers=false"></iframe>

From this, we can see that we're not doing too bad for stability in GHC directly.
Many of these patches are for older changes for example.
Although, from first hand experience, I know we can definitely improve! 
We're going to extend this work by labeling the exact cause of
breakage for each patch (many do overlap).
This will allow us to see more clearly which changes caused most
issues and help us prevent similar situations in the future.

Do you have any opinions on this document? 
do you feel stability could be better? or do you have a specific frustration which grinds your gears around stability?
Please reach out! 
We're always interested in hearing from the community and are in fact actively recruiting
members.
You don't need to join; you can also email or use the comment box below.
