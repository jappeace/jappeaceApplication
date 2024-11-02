Title: Firmware lemons 🍋
Date: 2024.11.02
Category: reflection
OPTIONS: toc:nil
Tags: haskell, programming, tools, reflex, frp, servant
subreddit: haskell programming reflexfrp

<style>
img[src="images/2024/calc-gamble.png"]{
  height: 20em;
  float: right;
  margin: 2em;
  width:unset;
}
</style>

![Life and lemons](images/2024/calc-gamble.png)

Ever found yourself fixing a problem you weren't hired to solve? 
To the point where you're under qualified?
That's what happened when I dove into firmware.
It was my suggestion, nobody *asked* me to do this,
but we needed better scanners, better hardware even,
and we couldn't get it due to politics.
They asked us to get more customers, 
before we could get better hardware.
Cost was the main issue, this project was perceived
as too expensive.
But the irony was that we needed better
hardware to get more customers.
We had to break this deadlock.

I had never even seen any firmware before!
But it was pretty clear at this
point our Bluetooth scanners were underperforming.
They are the source of all data, 
and they'd catch out about 5 signals per second.
Which is far lower than what you can see on any Bluetooth capable device.
I worked at this place for about a year,
before this even came up.
We tried doing fancy startup things, and underachieved.
For example we wanted to detect if a "lift" was happening,
but only got a single observation from the scanner for a two minute
period.[^area-note]
About two months ago I brought that the scanner firmware maybe bad.
We thought the scanners worked at peak performance before that.
Kind of silly looking back.
We naively assumed we were set up for success,
but we were given lemons.

[^area-note]: Note that there would be many Bluetooth tags all emitting signals, 
              so even if the scanner could catch 5 a second,
              it had to send the right signal as well!
              Supposedly the Bluetooth tags were sending out a signal once
              every 300 milliseconds. so what happened to all these signals?
              What would happen in this 2 minute interval is that it'd
              pick up the wrong signals, of stuff just laying around, and
              send those over!

## Experiment
My colleague tracked down the packet size configuration and increased it as an experiment. 
This doubled the performance of the device.
I was relieved by this; I sensed the deadlock breaking and could smell lemonade.
If your goal is to figure out what's going in reality
with Bluetooth scanners, 
and you double the performance with a small firmware modification.
You know you're doing something right.

I took over from my colleague,
because I was there to write code.
Initially I was so bad at this that she 
had to bail me out a couple of times. 
With tasks such as flashing builds or 
dealing with bricked hardware.
The first week I essentially "did" nothing,
because I was just struggling with getting builds out
or busy reading the [docs](https://docs.particle.io/getting-started/device-os/introduction-to-device-os/)
and understanding what was going on.
If a device bricks due to faulty firmware (think segfaults), 
it frustratingly only shows a red blinking light.
No [GDB](https://sourceware.org/gdb/) for you.
You can no longer put new firmware on it either,
but there is a reset trick[^particle-one] for that.
My colleague recommended that I make a small, baby-step changes.
Test each small change out out on the device making sure it doesn't crash. [^1] 


[^particle-one]: For particle tracker one, you have to screw it open, and there is a mode and reset button, hold the mode button, then tap the reset button, once it starts flashing green release mode, this will allow you to put new firmware on it again via usb.

[^1]: Which is starkly different from my haskell workflow
    of just writing everything you want in one go,
    solve a metric ton of compile errors and having
    a functioning program.
    This Made me realize how spoiled I am.

## Sorting
Anyway, I got better.
In the second week I got a build that sorted on signal strength values. [^struggles]
We believed the machine was sending random updates, 
whereas we were mostly interested in strong signals. [^requested]
So by sorting we'd just get the best information
out of the machine, neglecting weak signals.
This again showed an improvement, not as good 
as the increased packet size, 
but a good step forward.

[^struggles]: Apparantly you can use some c++ standard library function, trying to use a custom sort from the internet also cost me days. 😅

[^requested]: My colleague menioned they asked to sort the signals from the original contractors, but they never did this.

## Mutex nonsense
The following week I tackled the concurrency issue.
I knew this was an issue because there was a [mutex](https://stackoverflow.com/questions/34524/what-is-a-mutex)
that excluded the Bluetooth scanner from
running while sending of packets was happening via the SIM card (and vice versa).
They put instructions on a thread, but nothing ran concurrently.
For example here is the scan thread doing the Bluetooth scanning:
```c++
void Beaconscanner::scan_thread(void *param) {
    while(true) {
        if (!_instance->_run) {
            os_thread_yield();
            continue;
        }
        custom_scan_params();
        if(_instance->_scan_done) { // mutex here disables the thread on true
            os_thread_yield();
            continue;
            }
        long int elapsed = millis();
        while(_instance->_run && millis() - elapsed < _instance->_scan_period*1000) {
            Vector<BleScanResult> cur_responses = BLE.scan();
            _instance->processScan(cur_responses);
        }
        _instance->_scan_done = true;
        os_thread_yield();
    }
}
```

and then we got the main loop consuming it:
```c++
void Beaconscanner::loop(bool publishScans) {
    ...
    if (_scan_done) { // mutex here, only publish on true
        publish_all();
        _scan_done = false;
    }
  }
```

You may wonder how these threads even communicate,
no handles are passed around.
The answer is of course global variables and the use of various locking macros.[^globals] 
Not that the locking matters because `_scan_done` is used as a mutex.
So we've two threads running,
but the relevant code never actually runs at
the same time.

> They wrote code, and then wrote other code to undo their initial code.

They took the effort of putting the Bluetooth scanner on a thread.
And then wrote a mutex to undo the threading.
It would've been more efficient if all was done on the main loop with no threading at all!
Once I realized this was happening I was baffled.
The author got paid good money to undo his own code with more code,
resulting in firmware like a sour lemon.

[^globals]: There was no reason for this and [it's](https://google.github.io/styleguide/cppguide.html#Static_and_Global_Variables) [suspicious](https://stackoverflow.com/a/485020).

I tried explaining this to my dear colleagues,
I said something like "we've threads but no concurrency".
Perhaps this was too dense,
but to be fair it's hard to explain nonsense.
Fortunately this weirdness was also visible in the observations. 
At times we were just getting no observations when we'd expect some.
By the time we were talking about these observations,
I already had a build, 
because I knew the code was nonsense and had addressed it.[^turnover]

[^turnover]: This explains the fast turnover time, I just fixed obvious glaring problems, which you can see by reading code and trying to figure out what it does.

## Fixing concurrency
The fix involved deleting it all.
Both the locks and the mutex. 
Instead I used a queue for inter thread communication.
Queues is doing threads on easy mode,
and I had no-one to impress,
so I did easy mode.
However, even in easy mode this was still quite challenging to do.
Partly due to my inexperience with C-like programming.
The queue I ended up using looked like [this](https://docs.particle.io/firmware/software-design/threading-explainer/?q=thread#os_queue_create):
```C
int os_queue_create(os_queue_t* queue, size_t item_size, size_t item_count, void* reserved);
```
It's a bit wack; you reserve some memory
for the queue, giving you a `os_queue_t*`.
Then you've to initialize that memory with `os_queue_create`.
This fills in some fields of the structure.
I got stuck on using `os_queue_put` and `os_queue_take` for a while, 
which caused me red blinking light grief[^rs-segfault] (segfaults):
```C
int os_queue_put(os_queue_t queue, const void* item, system_tick_t delay, void* reserved);
int os_queue_take(os_queue_t queue, void* item, system_tick_t delay, void* reserved);
```
I think the issue was that I tried putting in a [`BleScanResult`](https://docs.particle.io/reference/device-os/api/bluetooth-le-ble/blepeerdevice/#blescanresult) directly.
But it contains some pointers,
and this queue wants you to copy over all data as a continuous memory block.
`void*` in this case means give me whatever memory pointer, [^haskell-void]
and the `os_queue_create` function already told how large it should be.
So instead of putting everything on the queue, I just made a struct with
all relevant information:

[^rs-segfault]: Because the code segfaults and red blinking light is all you get.

[^haskell-void]: Not to be confused with `void`, which is just an empty return type, or `Void` in haskell, which means no value will ever be that.
```C
struct QueueItem {
    BleAddress address; 
    uint8_t advertiseData[BLE_MAX_ADV_DATA_LEN]; 
    size_t count; 
    int8_t rssi;
};
```
A struct is a continuous memory block.[^know-why]
`BleAddress` can be inserted because it is also a simple struct with no pointers.
I had to make some more modifications to support the primitives in the `Eddystone` class. [^class]
but once this was done it worked.

[^class]: with class we mean a c++ class, which is kindoff a module with implied mutable state. Or a struct, with member functions, it forces a construction function as well. 
[^know-why]: I think I only know this because I played around before with haskell to c FFI.

## Results: From lemons to lemonade
The results of these improvements are impressive:

| firmware          | p/s  |
|-------------------|------|
| lemons            | 1.4  |
| packets & sorting | 5.57 |
| unlocked          | 10.3 |

Here, 'lemons' is the original firmware; 'packets & sorting' is after sorting,
and 'unlocked' is me addressing the concurrency issues, by deleting the locks.
`p/s` stands for packets per second for strong signals in this case.
This is just a single measure for strong close signal bandwidth, 
but the new firmware improves in all categories we measured. [^force]
I did this in about three weeks of work.
Because we're going so fast, 
there are more improvements in the pipeline since we've time,
such as using Protobuf instead of JSON, which will increase throughput,
and tweaking Bluetooth parameters causing a higher capture rate,
which I may describe in a later post.
After we've done all that, we can say we've made lemonade!

[^force]: In the force update activity for examle, by just tapping the sensor to 
          a Bluetooth tag we managed to increase success rate from 20% to about 90%. [^doesntwork]
          The system only managed to work without doing this by having long exposure to
          signals, but now we also support these quick exposures.
          Which makes the entire stack more robust.

It's funny that I was unsure about starting with firmware
changes or even suggesting it.
I was outright anxious about doing these concurrency updates.
They all turned out to have outsized impacts on the entire tech stack.
We're winning now.
This company may very well become a logistics company due to these calculated gambles.
Do you have any stories about professional, calculated gambles?
Please let me know in the comments; I'd love to know 😀
