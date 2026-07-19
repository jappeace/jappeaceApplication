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

<script>
(function () {
  const root = document.getElementById('coin-flip-game');

  // Emoji as unicode escapes: keeps this file plain ASCII, so it renders
  // correctly even if the surrounding page is not served as UTF-8.
  root.innerHTML = `
    <h3>\u{1FA99} Rigged Coin Trader</h3>
    <div class="stats">
      <div>Target: $999</div>
      <div>Time left: <span class="timer">30:00</span></div>
    </div>
    <div class="progress-track"><div class="progress-fill"></div></div>
    <div>Total flips: <span class="flip-count">0</span></div>
    <div class="balance">$25.00</div>
    <div class="controls">
      <div class="quick-bets">
        Bet
        <button type="button" data-fraction="0.10">10%</button>
        <button type="button" data-fraction="0.20">20%</button>
        <button type="button" data-fraction="0.50">50%</button>
        <button type="button" data-fraction="1.00">Max</button>
        of balance
      </div>
      <div>
        <label>Bet $: <input class="bet-amount" type="number" step="0.01" min="0.01" value="0.00"></label>
      </div>
      <div class="gamble-actions">
        <button type="button" data-choice="heads">Bet HEADS (60%)</button>
        <button type="button" data-choice="tails">Bet TAILS (40%)</button>
      </div>
    </div>
    <div class="game-over" style="display: none;"></div>
    <div class="log"><div><em>Heads hits 60%, tails 40%. The clock starts on your first bet. Good luck!</em></div></div>
  `;

  const startingBalance = 25.00;
  const targetBalance = 999.00;
  const headsProbability = 0.60;
  const timeLimitSeconds = 30 * 60;

  let balance = startingBalance;
  let timeRemaining = timeLimitSeconds;
  let gameActive = true;
  let timerInterval = null;
  let flipCount = 0;

  const balanceDisplay = root.querySelector('.balance');
  const timerDisplay = root.querySelector('.timer');
  const progressFill = root.querySelector('.progress-fill');
  const betInput = root.querySelector('.bet-amount');
  const logDiv = root.querySelector('.log');
  const controls = root.querySelector('.controls');
  const gameOverScreen = root.querySelector('.game-over');
  const flipCountDisplay = root.querySelector('.flip-count');

  function updateTimer() {
    if (!gameActive) return;

    timeRemaining--;
    const minutes = Math.floor(timeRemaining / 60);
    const seconds = timeRemaining % 60;
    timerDisplay.textContent = `${minutes}:${seconds < 10 ? '0' : ''}${seconds}`;

    if (timeRemaining <= 0) {
      endGame("Time's up! You failed to reach the target.", false);
    }
  }

  function setBetFraction(fraction) {
    if (!gameActive) return;
    let amount = Math.floor(balance * fraction * 100) / 100;
    if (amount <= 0 && balance > 0) amount = 0.01;
    betInput.value = amount.toFixed(2);
  }

  function gamble(playerChoice) {
    if (!gameActive) return;

    const bet = parseFloat(betInput.value);

    if (isNaN(bet) || bet <= 0) {
      alert("Please enter a valid bet amount greater than 0.");
      return;
    }
    if (bet > balance) {
      alert("You cannot bet more than your current balance!");
      return;
    }

    // The clock only starts once the reader actually plays, not on page load.
    if (timerInterval === null) {
      timerInterval = setInterval(updateTimer, 1000);
    }

    flipCount++;

    const landedHeads = Math.random() < headsProbability;
    const landedSide = landedHeads ? 'Heads' : 'Tails';
    const isWin = (playerChoice === 'heads' && landedHeads)
               || (playerChoice === 'tails' && !landedHeads);

    if (isWin) {
      balance += bet;
      logAction(`Landed ${landedSide}! You won $${bet.toFixed(2)}`, true);
    } else {
      balance -= bet;
      logAction(`Landed ${landedSide}! You lost $${bet.toFixed(2)}`, false);
    }

    // Fix floating point precision
    balance = Math.round(balance * 100) / 100;

    updateUI();
    checkWinLoss();
  }

  function updateUI() {
    balanceDisplay.textContent = `$${balance.toFixed(2)}`;
    flipCountDisplay.textContent = flipCount;
    const progressPercent = Math.min((balance / targetBalance) * 100, 100);
    progressFill.style.width = `${progressPercent}%`;

    if (parseFloat(betInput.value) > balance) {
      betInput.value = balance.toFixed(2);
    }
  }

  function logAction(message, isWin) {
    const entry = document.createElement('div');
    entry.className = isWin ? 'win-text' : 'lose-text';
    entry.textContent = message;
    logDiv.prepend(entry);
  }

  function checkWinLoss() {
    if (balance >= targetBalance) {
      endGame(`\u{1F389} YOU WIN! You reached $${balance.toFixed(2)}!`, true);
    } else if (balance <= 0) {
      balance = 0;
      endGame("\u{1F480} REKT! You hit $0.00. Bankrupt.", false);
    }
  }

  function endGame(message, isWin) {
    gameActive = false;
    clearInterval(timerInterval);
    controls.style.display = 'none';
    gameOverScreen.style.display = 'block';
    gameOverScreen.innerHTML = `
      ${message}
      <div>It took you exactly <strong>${flipCount}</strong> presses to get here.</div>
    `;
    gameOverScreen.className = `game-over ${isWin ? 'win-text' : 'lose-text'}`;
    updateUI();
  }

  root.querySelectorAll('[data-fraction]').forEach(function (button) {
    button.addEventListener('click', function () {
      setBetFraction(parseFloat(button.dataset.fraction));
    });
  });
  root.querySelectorAll('[data-choice]').forEach(function (button) {
    button.addEventListener('click', function () {
      gamble(button.dataset.choice);
    });
  });

  updateUI();
})();
</script>

There is a decent
chance you went bust.
If you went bust, this is the most valuable investment lesson you'll learn at a bargain price of free.

This [experiment](https://elmwealth.com/lessons-from-betting-on-a-biased-coin-cool-heads-and-cautionary-tales/)
was run with finance students and young professionals, and apparently 28% of them managed
to go bust as well, so don't feel bad.
Two thirds of the players even bet on tails at some point.
Everyone knew heads was the better bet, and they fucked it up anyway.

The trick is bet sizing. 
I mean the entire game is bet sizing so it may seem a bit silly as an observation. 
All you have to do is place small bets and hit the 60% win button.
There is an optimal sizing which you can calculate via the [Kelly criterion](https://en.wikipedia.org/wiki/Kelly_criterion).
In this case the optimal size is 20%: `2*60%-1 = 20%` 
although I found it easier to just use 10%
and keep smashing, occasionally resizing to 10% on significant balance changes.
The criterion says: Too small sizing is fine, but too big sizing can lead to ruin.
My 10% strategy gave me some leeway, 
allowing a couple losses before the winnings came in again.

Bet sizing is a fundamental law in investing.
Even with a good edge like the above game, you may still lose if you do this wrong.
It's fine to take on some risk, just don't go put everything on black,
which will lead to [gambler's ruin](https://en.wikipedia.org/wiki/Gambler%27s_ruin).


Ironically enough a lot of software jobs' incentive structures with equity
break this fundamental law as well.
If you get awarded options for a startup, the mathematics says that, despite the fat equity payouts, 
you probably should leave after a while anyway just to diversify.[^other-problems]
Because if you have all your options on a single company, 
it's like doing one *big* bet on that company.
So like the above game shows, you want to diversify. 
Which is made pretty hard by most incentive schemes.
Allowing employees to trade their options with those of other startups would help this problem. 
All of the schemes I've seen ban any peer to peer trading.
So the only thing you can do is switch jobs after a while.
I'm not sure if people are aware that the incentive structure meant to keep
you there does the opposite.

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
