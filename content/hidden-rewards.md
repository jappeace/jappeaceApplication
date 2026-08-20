Title: Hidden rewards
Date: 2026-08-19 22:30
Category: reflection
OPTIONS: toc:nil
Tags: gambling, stocks

This time one of the buttons has an upside, 
I won't tell which. Good luck!

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
  #coin-flip-game .tally { font-weight: bold; }
  #coin-flip-game .shop {
    border-top: 1px dashed #196019;
    padding-top: 0.6em;
    display: grid;
    gap: 0.4em;
    text-align: left;
  }
  #coin-flip-game .shop-header { font-weight: bold; }
  #coin-flip-game .shop-item {
    display: flex;
    justify-content: space-between;
    gap: 1em;
    width: 100%;
  }
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
  #coin-flip-game .game-over a { color: inherit; text-decoration: underline; }
  @media (prefers-color-scheme: dark) {
    #coin-flip-game { background: rgba(0, 0, 0, 0.85); }
    #coin-flip-game .progress-track { background: #134013; }
    #coin-flip-game .win-text { color: #7dff7d; }
    #coin-flip-game .lose-text { color: #ff6b6b; }
  }
</style>

<script src="/coin-flip-level2.js"></script>
<script>
Elm.CoinFlipLevel2.init({ node: document.getElementById('coin-flip-game') });
</script>

Games in general become more interesting with hidden information.
The strategy here is to just bet as little as possible, and only go for [kelly criterion](https://jappie.me/even-with-an-edge-you-lose.html) once you know 
which button wins.
I thought this would be a bit tedious todo so I added a feature to keep track of win lose ratios,
at a relatively steep cost.
I did this intentionally, but it's basically a win button for those who are "kelly aware" (and to lazy to track by hand).
To keep it interesting it varies a bit between 55% and 65% (for the winning button).

