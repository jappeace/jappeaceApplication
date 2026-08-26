Title: Correlation implies cashation.
Date: 2026-08-22 21:00
Category: reflection
OPTIONS: toc:nil
Tags: gambling, finance, game
Summary: Figure out how these birds relate and harvest the gravy!

Look at you, you've become quite the degenerate gambler, nice!
Anyway after seeing what you did in [level 3: black swan](/black-swan.html), 
I decided to put a turn limit on this game.
Your previous strategies won't work due to the turn limit.
You'll have to figure out how these birds relate,
once you do that, you can just harvest the gravy.

<div id="coin-flip-game"></div>

<link rel="stylesheet" href="/theme/css/coin-flip-game.css">

<script src="/coin-flip-level4.js"></script>
<script>
var coinFlipApp = Elm.CoinFlipLevel4.init({
  node: document.getElementById('coin-flip-game'),
  flags: Math.floor(Math.random() * 1000000000)
});
if (coinFlipApp.ports && coinFlipApp.ports.gameAnalyticsEvent) {
  coinFlipApp.ports.gameAnalyticsEvent.subscribe(function (event) {
    if (window.gtag) { gtag('event', event.name, event.params || {}); }
  });
}
</script>

This level is (again) a lot harder than the previous ones.
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
staked on a bird with win probability $p$ and net odds $b$, the
profit on a winning dollar: the table's payout column is $1 + b$, the
stake coming back plus the winnings, so rainy's 2.8x means $b = 1.8$.
The bet collapses to two outcomes, win $+b$ or lose $-1$:

$$\mathbb{E} = p \cdot b - q$$

with $q = 1 - p$ the chance of losing. For rainy:
$0.4 \cdot 1.8 - 0.6 = 0.12$, the table's number.

</details>

Now suppose you did the maths, found out rainy has the best expected
value, and used what you've learned in previous levels.
You'll run into another trap, let's figure out an allocation size for rainy:

$$f^* = \frac{bp - q}{b}$$

where:

* $f^*$ is the fraction of your current balance to stake on each flip,
* $p$ is the probability that the bet wins,
* $q = 1 - p$ is the probability that it loses,
* $b$ is the net odds: the profit per dollar staked on a win.

$$f^* = \frac{1.8 \cdot 0.4 - 0.6}{1.8} \approx 0.0666$$

So you can safely stake 6.7% on rainy, giving you an expected arithmetic gain per flip of $6.7\% \cdot 0.12 \approx 0.8\%$.
This number is an optimistic estimation of growth and if you plug it in as a growth rate on the starting
balance of $25 you need around 450 turns to complete.
What is missing is [volatility drag](https://en.wikipedia.org/wiki/Volatility_tax)
which drags the expected return down to 0.39%, requiring around 950 flips to get to $999.
The scenario is mathematically impossible with a turn limit of 200, which is intentional:
I want you to look for the correlation, or rather, de-correlation.


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
Going up by 12% and then down by 12% doesn't leave you at the same spot,
you end up at 1.4% lower than your starting position.
This is the volatility tax, formalized as:

$$g^* \approx \mu - \frac{\sigma^2}{2}$$

*where:*

* *$g^*$ is the growth rate you actually compound at per flip: the
  average of $\ln(\text{balance after}/\text{balance before})$, which
  for rainy at the kelly stake comes out at the 0.39\% above,*
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

This works because the game is rigged in your favor.
Either the sunny bird or the rainy bird will win, they
are negatively correlated.

Suppose sunny has a chance of 60% to win, 
conversely the rainy coin has a 40% chance to win, because when sunny doesn't win, rainy will win.
Now look at the pricing of these, this is where the unfairness lives:

- Sunny coin, $b_1 = 0.8$: implied $\frac{1}{1.8} = 55.6\%$ (true chance: 60%)
- Rainy coin, $b_2 = 1.8$: implied $\frac{1}{2.8} = 35.7\%$ (true chance: 40%)
- Implied sum: 91.3%. True sum: 100%

Here implied means the probability you'd get from just looking at the prices, it's the "fair" odds.
If the sum of all implied odds doesn't reach 100% the game is superfair (rigged in your favor).
If we put all our money split across the true chance we'll expect an 8.7 cent harvest yielding $\frac{1}{0.9127} - 1 \approx 9.6\%$ per flip.
And there is no reason not to use all our money, 
because you'll either win 1.8 times your stake 
or you'll win 2.8 times your stake.
Of course you'll lose the part of the stake you put on the losing
bird, so you end up expecting around $9.6\%$ per flip anyway.
The 60/40 split is to maximize average growth, the kelly point.[^kelly]

This isn't quite the same as a money pump, or sure bet.
The Kelly point maximizes the average growth rate[^ev-note], 
whereas sure bet gives no variance (volatility) on payoff[^avoiding-volatility-tax].
If you want an actual sure bet, 
you have to use the implied percentages
which add up to 91.3.
Now we can just divide:

$$55.6 + 35.7 = 91.3 \qquad \frac{55.6}{91.3} = 60.9\% \qquad \frac{35.7}{91.3} = 39.1\%$$

So 61% on sunny, 39% on rainy,
kelly and the sure bet almost converge. 
This is a coincidence of how this game was set up.
If we for example change the payoff, double the sunny outcome,
then the kelly point remains the same, but the sure bet will
drift towards 50/50.

[^avoiding-volatility-tax]: Note that with no variance you avoid the volatility tax! Your average growth is lower but without variance you can bet with more money then you own. This gives me more scenario ideas :)

[^ev-note]: Note that kelly is different from maximizing expected value.
    The rainy bird in fact has the highest expected value, 
    so you should go all in on that if you want to get more expected value.
    The problem is that you'd be ruined by the time you reach the end of the game.
    Expected value doesn't work in repeated games.

## Do money pumps exist?

You may ask yourself, does a money pump exist in real life.
The answer is YES. 
Risk free money is real, and also boring.
For example: your bank account gives your risk free interest payments, 
or if you want to be more creative you can buy a government bond.
You just don't get to "flip" as often as in the game above.

Adding risk to lose is what makes the above game interesting.
But the payout per "flip" is also much better.
I made you work to find the money pump,
do it slightly wrong and you'll lose.
In real life there are also more "risky" examples of people trying to make money pumps.
Hedge funds try to find similar decorrelations[^complex].
If they do it slightly wrong they'll also lose,
a recent example of loss is the [situational awareness](https://aswathdamodaran.substack.com/p/the-situational-awareness-fund-blow) 
blow up.
Something interesting about that loss is that the wider economy doesn't
actually care, 
because in these stock market like trading games, if someone loses, another person wins:
It's zero-sum.

[^kelly]: J.L. Kelly Jr., "A New Interpretation of Information Rate",
Bell System Technical Journal, 1956; Cover & Thomas, *Elements of
Information Theory*, ch. 6: proportional betting is log-optimal, and
"a Dutch book, though risk-free, does not optimize the doubling rate."

[^complex]: Note that the real world is much more complex than what we do here. And they try to decorrelate not just via stocks of course.
