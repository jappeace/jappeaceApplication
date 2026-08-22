Title: Correlation implies cashation.
Date: 2026-08-21 22:30
Category: reflection
OPTIONS: toc:nil
Tags: gambling, finance, game
Summary: Three birds, one sky, two hundred flips. Good luck!

Look at you, you're become quite the degenerate gambler, nice!
Anyway after seeing what you did in [level 3: black swan](/black-swan.html), 
I decided to put a turn limit on this game.
Your previous strategies wont work due to the turn limit.
You'll have to figure out how these birds relate,
once you do that, you can just harvest the gravy.

<div id="coin-flip-game"></div>

<style>
  #coin-flip-game {
    border: 2px solid #196019;
    border-radius: 6px;
    overflow: hidden;
    max-width: 34em;
    margin: 1.5em auto;
    padding: 0 1em 1em;
    font-family: "Inconsolata", "DejaVu Sans Mono", "Bitstream Vera Sans Mono", monospace;
    background: rgba(255, 255, 255, 0.85);
  }
  #coin-flip-game h3 {
    background: #196019;
    color: white;
    margin: 0 -1em 1em;
    padding: 0.4em 1em;
    font-size: 1.1em;
  }
  #coin-flip-game .stats { display: flex; justify-content: space-between; font-weight: bold; }
  #coin-flip-game .progress-track {
    background: #d3d7cf;
    height: 12px;
    margin: 0.5em 0;
    border-radius: 6px;
    overflow: hidden;
  }
  #coin-flip-game .progress-fill { background: #30bb30; height: 100%; transition: width 0.3s; }
  #coin-flip-game .balance { font-size: 2.2em; font-weight: bold; text-align: center; margin: 0.3em 0; }
  #coin-flip-game .controls { text-align: center; }
  #coin-flip-game .controls > div { margin: 0.6em 0; }
  #coin-flip-game button {
    touch-action: manipulation;
    font-family: inherit;
    background: #196019;
    color: white;
    border: none;
    border-radius: 4px;
    padding: 0.25em 0.7em;
    cursor: pointer;
  }
  #coin-flip-game button:hover { background: #259025; }
  #coin-flip-game button:active { transform: scale(0.97); }
  #coin-flip-game .coin-bet {
    display: flex;
    justify-content: space-between;
    align-items: center;
    gap: 1em;
  }
  #coin-flip-game .coin-bet label { font-weight: bold; }
  #coin-flip-game .flip-button { width: 100%; padding: 0.6em; font-weight: bold; }
  #coin-flip-game .tally { font-weight: bold; }
  #coin-flip-game .glasses { font-weight: bold; }
  #coin-flip-game .correlations { font-weight: bold; }
  #coin-flip-game .shop {
    border-top: 1px dashed #196019;
    padding-top: 0.6em;
    display: grid;
    gap: 0.4em;
    text-align: left;
  }
  #coin-flip-game .shop-toggle { width: 100%; text-align: left; font-weight: bold; }
  #coin-flip-game .shop-group-heading { font-weight: bold; }
  #coin-flip-game .shop-item {
    display: flex;
    justify-content: space-between;
    gap: 1em;
    margin-left: 1.2em;
  }
  #coin-flip-game .purchase-dialog {
    position: fixed;
    z-index: 10;
    width: 20em;
    border: 1px solid #196019;
    border-radius: 4px;
    padding: 0.6em;
    text-align: left;
    background: #eafbea;
  }
  #coin-flip-game .dialog-actions { display: flex; gap: 0.6em; margin-bottom: 0.5em; }
  #coin-flip-game input[type="number"] {
    font-family: inherit;
    border: 1px solid #196019;
    border-radius: 4px;
    padding: 0.3em 0.5em;
    width: 7em;
    background: transparent;
    color: inherit;
  }
  #coin-flip-game .log {
    height: 8em;
    overflow-y: auto;
    font-size: 0.85em;
    text-align: left;
    border-top: 1px dashed #196019;
    padding-top: 0.5em;
    margin-top: 0.8em;
  }
  #coin-flip-game .log-divider { border-top: 1px solid #333; margin: 0.25em 0; }
  #coin-flip-game .win-text { color: #1e7d1e; }
  #coin-flip-game .lose-text { color: #b03030; }
  #coin-flip-game .game-over { font-weight: bold; text-align: center; font-size: 1.1em; margin: 0.5em 0; }
  #coin-flip-game .game-over a { color: inherit; text-decoration: underline; }
  @media (prefers-color-scheme: dark) {
    #coin-flip-game { background: rgba(0, 0, 0, 0.85); }
    #coin-flip-game .progress-track { background: #134013; }
    #coin-flip-game .log-divider { border-color: #888; }
    #coin-flip-game .win-text { color: #7dff7d; }
    #coin-flip-game .lose-text { color: #ff6b6b; }
    #coin-flip-game .purchase-dialog { background: #062606; }
  }
</style>

<script src="/coin-flip-level4.js"></script>
<script>
Elm.CoinFlipLevel4.init({ node: document.getElementById('coin-flip-game') });
</script>

This level is (again) a lot harder then the previous ones.
No longer is it sufficient to spot a winning bird and just size right,
you've to come up with a real allocation strategy.
However once you do, this is a [money pump](https://en.wikipedia.org/wiki/Dutch_book).
If you lost, don't feel bad, I'll explain the trick and you can try again.

First you need to figure out the probabilities and payouts (as usual).
I just did a couple 10ct bets on each bird to get the payouts.
For the probabilities I just used the tracker, 
20 bets or so give you all the information.
There are 3 bird profiles, sunny bird, rainy bird and the red herring.

| name        | probability | payout | expected value |
|-------------|-------------|--------|----------------|
| red herring | 2%          | 41x    | -0.18          |
| sunny       | 60%         | 1.8x   | 0.08           |
| rainy       | 40%         | 2.8x   | 0.12           |

So we can just ignore the red herring because it has a negative expected value.
It's there to trap players who won the [previous level](/black-swan.html)
and assume betting on low odds high payout always wins.

<details>
<summary>Expected value</summary>

$$\mathbb{E}[X] = \sum_i P(x_i) \cdot x_i$$

where:

* $\mathbb{E}[X]$ reads "the expected value of $X$". The
  double-struck $\mathbb{E}$ is the expectation operator, a machine
  that eats a random thing and spits out one number, and the square
  brackets are the notation for feeding it that thing. They are not
  multiplication: $\mathbb{E}$ on its own is nothing, like how
  $\sqrt{\phantom{x}}$ needs something under it.
* $X$ is the bet, a random variable: the thing whose outcomes chance
  picks between,
* $x_i$ are the possible outcomes, as won or lost money,
* $P(x_i)$ is the probability of outcome $x_i$.

Each outcome's value weighted by its probability. For one dollar
staked on a bird with win probability $p$ and net odds $b$ (the
profit on a winning dollar, so the table's payout column minus the
returned stake: $1 + b$) it collapses to two outcomes, win $+b$ or
lose $-1$:

$$\mathbb{E} = p \cdot b - q$$

with $q = 1 - p$ the chance of losing. For rainy:
$0.4 \cdot 1.8 - 0.6 = 0.12$, the table's number.

</details>

Now if you did the maths and found out the best expected value rainy and use
what you've learned in previous levels. 
You'll run into another trap, let's figure out an allocation size for rainy:

$$f^* = \frac{bp - q}{b}$$

where:

* $f^*$ is the fraction of your current balance to stake on each flip,
* $p$ is the probability that the bet wins,
* $q = 1 - p$ is the probability that it loses,
* $b$ is the net odds: the profit per dollar staked on a win.

$$f^* = \frac{1.8 \cdot 0.4 - 0.6}{1.8} \approx 0.0666$$

So you can safely stake 6.7% on rainy, giving you an expected arithmetic gain per flip of $6.7% * 0.12 \aprox 0.8%$
This number is an optimistic estimation of growth and if you plug it in as a growth rate on the starting
balence of $25 you need around 450 turns to complete.
What is missing is [volatility drag](https://en.wikipedia.org/wiki/Volatility_tax)
which yields an expected return instead of 0.39%, requiring around 950 flips to get to $999.
The scenario is mathematically impossible with a turn limit of 200, which is intentional!
This is intentional because I want you to look for the correlation, or rather, de-correlation.


<details>
<summary>Volatility Drag</summary>
For example:

| swing              | multipliers          | after both flips |
|--------------------|----------------------|------------------|
| up 12\%, down 12\% | $1.12 \times 0.88$   | $-1.4\%$         |
| up 30\%, down 30\% | $1.30 \times 0.70$   | $-9.0\%$         |

Even though you go up and down by the same percentage, 
you pay a tax due to how multiplication 
works on carried bases.
This is formalized as:

$$g^* \approx \mu - \frac{\sigma^2}{2}$$

*where:*

* *$\mu$ is the arithmetic average return per flip (our 0.8\%),*
* *$\sigma^2$ is the variance of that return, how hard the flips
  swing around their average,*
* *$\sigma^2 / 2$ is the volatility drag, the toll from the curved
  logarithm ($\ln(1+x) \approx x - x^2/2$).*

</details>


We've to use negative correlation cleverly.
Two of these birds always land on opposites.
Furthermore they pay out sufficiently each to cover the loss of the other.
So you can just put your betting percentage on whichever birds
are negatively correlated and win.
This is known as the kelly point.
This works because the game is rigged in your favor.
Either the sunny bird or the rainy bird will win, they
are negatively correlated.

Suppose sunny has a chance of 60% to win, 
conversely the rainy coin has a 40% chance to win, because when sunny doesn't win, rainy will win.
Now look at the pricing of these, this is where the unfairness lives:

- Sunny coin, $b_1 = 0.8$: implied $\frac{1}{1.8} = 55.6%$ (true chance: 60%)
- Rainy coin, $b_2 = 1.8$: implied $\frac{1}{2.8} = 35.7%$ (true chance: 40%)
- Implied sum: 91.3%. True sum: 100%

Here implied means the probability you'd get from just looking at the prices, it's the "fair" odds.
If the sum of all implied odds doesn't reach 100% the game is supper fair (rigged in your favor).
If we put all our money split across the true chance we'll expect a 8.7 cent harvest yielding $\frac{1}{0.913} - 1 = 9.6\%$ per flip.
And there is no reason not to use all our money, 
because you'll either win 1.8 times your stake 
or you'll win 2.8 times your stake.
Of course you'll lose the part of the stake you put on the losing
bird, so you end up expect around $9.6\%$ per flip anyway.
The 60/40 split is to maximize average growth, the kelly point.


Now if you want an actual sure bet, 
make the outcome of the
bet irrelevant and just harvest you have to use the implied percentages
which add up to 91.3.
Now we can just divide:

$$55.6 + 35.7 = 91.3 \qquad \frac{55.6}{91.3} = 60.9% \qquad \frac{35.7}{91.3} = 39.1%$$

So 61% on sunny, 39% on rainy,
kelly and the sure bet almost converge. 
This is a coincidence of how this game was setup.
If we for example change the payoff, double the sunny outcome,
then the kelly point remains the same, but the surebet will
drift towards 50/50.
Kelly maximizes the average growth rate[^ev-note], 
whereas surebet gives no variance (volatility) on payoff.

[^ev-note]: Note that kelly is different from maximizing expected value.
    the Sunny bird in fact has the highest expectd value, 
    so you should go all in on that if you want to get more expected value.
    The problem is that you'd be ruined by the time you reach the end of the game.
    Expected value doesn't work in repeated games.



## Do money pumps exist?

You may ask yourself, does a money pump exist in real life.
The anwser is YES. 
Risk free money is real, and also boring.
For example: your bank account gives your risk free interest payments, 
or if you want to be more creative you can buy a government bond.
You just don't get to "flip" as often as in the game above.

Adding risk to lose is what makes the above game interesting.
But the payout per "flip" is also much better.
The game makes you I made you work to find the money pump,
do it slightly wrong and you'll lose.
In real life there are also more "risky" examples of people trying to make money pumps.
Hedge fund try to find similar discoloration in the stock market.
If they do it slightly wrong they'll also lose,
a recent example of loss is the [situational awareness](https://aswathdamodaran.substack.com/p/the-situational-awareness-fund-blow) 
blow up.
Something interesting about that loss is that the wider economy doesn't
actually care, 
because in these stock market like trading games, if someone loses, another person wins:
It's zero-sum.



## Maths

Where does that come from? Kelly again, but the situation changed:
this time the whole balance goes in, split across two outcomes of
which exactly one wins. Put fraction $x$ on sunny and $1-x$ on rainy.
A flip multiplies your balance by $(1+b_1)\,x$ when sunny, or by
$(1+b_2)(1-x)$ when rainy, so the long-run growth to maximize is

$$g(x) = p \cdot \ln\big((1+b_1)\,x\big) + q \cdot \ln\big((1+b_2)(1-x)\big)$$

where:

* $p$ is the true probability of sun, and $q = 1 - p$ that of rain
* $b_1$ is the net odds on the sun bird: a dollar bet returns $1 + b_1$
  dollars on a win, the stake plus $b_1$ profit (same $b$ as in
  level 1's $f^* = p - q/b$)
* $b_2$ is the same for the rain bird
* $x$ is the fraction of your balance on
  sunny, leaving $1-x$ for rainy
* $g(x)$ is the long-run growth rate: the average of
  $\ln(\text{balance after} / \text{balance before})$ per flip, the
  same quantity earlier levels compounded

The logarithm splits products into sums:

$$g(x) = \underbrace{p\ln(1+b_1) + q\ln(1+b_2)}_{\text{the payouts}}
       + \underbrace{p\ln x + q\ln(1-x)}_{\text{your split}}$$

Look at the first bracket: it contains no $x$. Every cent lands on
some bird, so the payouts multiply your wealth however you split;
they decide how much you grow, never how to divide. Only the second
bracket is yours to optimize, and setting its derivative to zero,

$$\frac{p}{x} - \frac{q}{1-x} = 0 \qquad\Rightarrow\qquad x = p$$

Bet your beliefs: the optimal split is the probabilities themselves,
and the payouts vanished from the answer. This is Kelly's original
horse-race result[^kelly]. Mind the fine print though: it only holds
because the bets cover every outcome and all the money is in. The
earlier levels were a single bet with cash on the side, which is why
their Kelly was $f^* = p - q/b$ and the payout mattered a great deal.

[^kelly]: J.L. Kelly Jr., "A New Interpretation of Information Rate",
Bell System Technical Journal, 1956; Cover & Thomas, *Elements of
Information Theory*, ch. 6: proportional betting is log-optimal, and
"a Dutch book, though risk-free, does not optimize the doubling rate."
