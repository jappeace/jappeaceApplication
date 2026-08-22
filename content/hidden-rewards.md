Title: Hidden rewards
Date: 2026-08-20 16:46
Category: reflection
OPTIONS: toc:nil
Tags: gambling, finance, game
Summary: This time one of the buttons has an upside, I won't tell which. Good luck!

[This time](https://jappie.me/even-with-an-edge-you-lose.html) one of the buttons has an upside, 
I won't tell which. Good luck!

<div id="coin-flip-game"></div>

<link rel="stylesheet" href="/theme/css/coin-flip-game.css">

<script src="/coin-flip-level2.js"></script>
<script>
Elm.CoinFlipLevel2.init({ node: document.getElementById('coin-flip-game') });
</script>

Games in general become more interesting with hidden information.
The strategy here is to just bet as little as possible, and only go for the [Kelly criterion](https://en.wikipedia.org/wiki/Kelly_criterion) once you know 
which button wins.
I thought this would be a bit tedious to do so I added a feature to keep track of win/lose ratios,
at a relatively steep cost.
The steep price is intentional, it's basically a win button for those who are "Kelly aware" (and too lazy to track by hand).
However, a gambler may see the steep cost and decide the information is not "worth" the $15 in bets he can place.
Uncle's advice is worth its weight in gold, of course.
