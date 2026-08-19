Title: Even with an edge, you lose?
Date: 2026-07-19 22:30
Category: reflection
OPTIONS: toc:nil
Tags: gambling, stocks

I rigged the coin such that it lands heads 60% of the time.
All you have to do is place your bet and start harvesting.
The goal of the game is to reach $999 within 30 minutes.
Try it out:

<div id="coin-flip-game"></div>

<style>
  #coin-flip-game {
    border: 2px solid #196019;
    border-radius: 6px;
    overflow: hidden;
    max-width: 26em;
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
  #coin-flip-game .gamble-actions { display: flex; gap: 0.6em; }
  #coin-flip-game .gamble-actions button { flex: 1; padding: 0.6em; font-weight: bold; }
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
  #coin-flip-game .win-text { color: #1e7d1e; }
  #coin-flip-game .lose-text { color: #b03030; }
  #coin-flip-game .game-over { font-weight: bold; text-align: center; font-size: 1.1em; margin: 0.5em 0; }
  @media (prefers-color-scheme: dark) {
    #coin-flip-game { background: rgba(0, 0, 0, 0.85); }
    #coin-flip-game .progress-track { background: #134013; }
    #coin-flip-game .win-text { color: #7dff7d; }
    #coin-flip-game .lose-text { color: #ff6b6b; }
  }
</style>

<script src="/coin-flip-level1.js"></script>
<script>
Elm.CoinFlipLevel1.init({ node: document.getElementById('coin-flip-game') });
</script>

There is a decent
chance you went bust.
If you went bust, this is the most valuable investment lesson you'll learn at a bargain price of free.

This [experiment](https://elmwealth.com/lessons-from-betting-on-a-biased-coin-cool-heads-and-cautionary-tales/)
was run with finance students and young professionals, and apparently 28% of them managed
to go bust as well, so don't feel bad.
Two thirds of the players even bet on tails at some point.
Everyone knew heads was the better bet, and they fucked it up anyway.[^why]

The trick is bet sizing. 
I mean the entire game is bet sizing so it may seem a bit silly as an observation. 
All you have to do is place small bets and hit the 60% win button.
There is an optimal sizing which you can calculate via the [Kelly criterion](https://en.wikipedia.org/wiki/Kelly_criterion).
In this case the optimal size is 20%: `2*60%-1 = 20%`[^one-one] 
although I found it easier to just use 10%
and keep smashing, occasionally resizing to 10% on significant balance changes.
The criterion says: Too small sizing is fine, but too big sizing can lead to ruin.
My 10% strategy gave me some leeway, 
allowing a couple losses before the winnings came in again.

Bet sizing is a fundamental law in investing.
Even with a good edge like the above game, you may still lose if you do this wrong.
It's fine to take on some risk, just don't go put everything on black,
which will lead to [gambler's ruin](https://en.wikipedia.org/wiki/Gambler%27s_ruin).


Ironically enough, a lot of software jobs' incentive structures with equity
break this fundamental law as well.
If you get awarded options for a startup, the mathematics says that, despite the fat equity payouts, 
you probably should leave after a while anyway just to diversify.[^other-problems]
Because if you have all your options on a single company, 
it's like making one *big* bet on that company.
So like the above game shows, you want to diversify. 
Which is made pretty hard by most incentive schemes.
Allowing employees to trade their options with those of other startups would help with this problem. 
All of the schemes I've seen ban any peer to peer trading.
So the only thing you can do is switch jobs after a while.[^correlation]
I'm not sure if people are aware that the incentive structure meant to keep
you there does the opposite.

[^correlation]: There is another big problem in that both your savings (equity) and your income are now correlated to the success of that one company. If the startup goes under, you don't just lose your job but also a ton of money. I suppose this is intentional however, because your wealth is aligned with the founders'. If a company gives you shares for whatever reason, the best thing you can almost always do is sell them and buy shares of companies you don't work for, preferably in a different sector.

[^other-problems]: Ignoring the many other problems these option structures have,
  such as: 
  these are complicated contracts which you probably shouldn't even be trading,
  and companies put all kinds of conditions within the contract that are unfair and 
  frankly crazy compared to normal money (vesting periods, clawback clauses, etc.).

I made this post because I wanted to see how this game actually worked,
after seeing it mentioned in [Patrick Boyle's video](https://www.youtube.com/watch?v=nJtL9MBVj48).
I was curious to know if I would win and how long it'd take, about 90 seconds apparently.
I'm not sure if the people in the experiment were allowed to smash the bet button like in my implementation,
but I'm pretty sure I'd have maxed out the original's $250 payout cap because I'm "Kelly aware".
If you went bust, I invite you to retry the above game now that you're "Kelly aware" too.
You can reset it by refreshing the page.

[^one-one]: Assuming a 1:1 payout.

[^why]: Aside from people just trying out the buttons, this could also be due to the [gambler's fallacy](https://en.wikipedia.org/wiki/Gambler%27s_fallacy), feeling that tails is "due".
