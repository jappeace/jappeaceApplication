Title: Black swan
Date: 2026-08-20 22:30
Category: reflection
OPTIONS: toc:nil
Tags: gambling, finance, game
Summary: Even the best gamblers fail on this level. Only a black swan of a gambler may succeed.

Even the best gamblers fail on this level.
Only a [black swan](https://en.wikipedia.org/wiki/The_Black_Swan:_The_Impact_of_the_Highly_Improbable) 
of a gambler may succeed.

<div id="coin-flip-game"></div>

<link rel="stylesheet" href="/theme/css/coin-flip-game.css">

<script src="/coin-flip-level3.js"></script>
<script>
var coinFlipApp = Elm.CoinFlipLevel3.init({
  node: document.getElementById('coin-flip-game'),
  flags: Math.floor(Math.random() * 1000000000)
});
if (coinFlipApp.ports && coinFlipApp.ports.gameAnalyticsEvent) {
  coinFlipApp.ports.gameAnalyticsEvent.subscribe(function (event) {
    if (window.gtag) { gtag('event', event.name, event.params || {}); }
  });
}
</script>

Here the way to win is to carefully study the log messages, 
you may have to place quite a few bets before you can even map the payouts to the birds.
You can try getting the golden glasses but you wouldn't have enough money for the tracker,
I think the tracker is more convenient than knowing the payouts.
Now you should calculate if it even makes sense to bet on these birds
once you've got their probabilities and payout factors.
This is just common sense.
Why make bets that have negative returns? 
It's like buying a lotto ticket or playing roulette, madness!
We don't play for [FUN](https://dwarffortresswiki.org/DF2014:Fun&redirect=no), we play to win!

In previous games every bet paid 1:1, which collapsed 
the Kelly formula to $2p-1$.
In this game we adjusted payout factors, 
so the Kelly fractions change accordingly.

$$f^* = p - \frac{q}{b} = \frac{bp - q}{b}$$

where:

* $f^*$ is the fraction of your current balance to stake on each flip,
* $p$ is the probability that the bet wins,
* $q = 1 - p$ is the probability that it loses,
* $b$ is the net odds: the profit per dollar staked on a win.
  A double-or-nothing bet has $b = 1$; a bet paying out 30 times the
  stake has $b = 30$. One of the birds only pays out 50%, for that one $b = 0.5$.

A negative $f^*$ means the edge is against you and the optimal stake is zero.
So whichever of those birds is the black swan[^random] has: $p = 0.05$, $q = 0.95$, $b = 30$:

$$f^* = 0.05 - \frac{0.95}{30} \approx 0.0183$$

I just used about 1% for smashing, for only the black swan, putting the other birds to zero because they had negative expected returns.
I didn't do the maths on my first run,
but I saw the low odds and high payout so I decided to do a tiny stake.
Anyway, if you can't figure all this shit out you can always ask uncle for advice, I'm sure he'll be proud of you.

After play testing for a bit I realized this level can be a ton of clicking,
so I added the auto clicker and flip helpers preventing endless mouse button bashing.

[^random]: It's random
