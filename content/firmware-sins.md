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

I wasn't hired to modify firmware.
But we needed better scanners, better hardware even,
and we couldn't get it due to politics.
They asked us to get more customers, 
before we could get better hardware.
Cost was the main issue, this project was perceived
as to expensive.
But the irony was that we needed better
hardware to get more customers.
We had to break this deadlock.

I never even seen any firmware before!
But it was pretty clear at this
point our Bluetooth scanners were underperforming.
They are the source of all data, 
and they'd catch out about 5 signals per second.
Which is far lower then what you can see on any Bluetooth capable device.
I worked at this place for about a year,
before this even came up.
We tried doing fancy startup things, and underachieved.
For example we wanted to detect if a "lift" was happening,
but only got a single observation from the scanner for a two minute
period.[^area-note]
About two months ago I brought up the issue of scanner firmware.
We thought the scanners worked at peak performance before that.
Kind of silly looking back.
We naively assumed we were setup for success,
but we were given lemons.

[^area-note]: Note that there would be many bleutooth tags all emitting signals, 
              so even if the scanner could catch 5 a second,
              it had to send the right signal as well!
              Supposedly the bleutooth tags were sending out a signal once
              every 300 milliseconds. so what happened to all these signals?
              What would happen in this 2 minute interval is that it'd
              pick up the wrong signals, of stuff just laying around, and
              send those over!

My colleague tracked down the packet size configuration and increased it as an experiment. 
This doubled the performance of the device.
I was relieved by this, I sensed the deadlock breaking and could smell lemonade.
If your goal is to figure out what's going in reality
with bleutooth scanners, 
and you double the performance with a small firmware modification.
You know you're doing something right.

I took over from my colleague,
because I was there to write code.
Initially I was so bad at this that she 
had to bail me out a couple times. 
With tasks such as flashing builds or 
dealing with bricked hardware.
The first week I essentially "did" nothing,
because I was just struggling with getting builds out
or busy reading the [docs](https://docs.particle.io/getting-started/device-os/introduction-to-device-os/)
and understanding what was going on.
If a device bricks due to faulty firmware (think segaults), 
it frustratingly only shows a red blinking light.
No [GDB](https://sourceware.org/gdb/) for you.
You can no longer put new firmware on it either,
but there is a reset trick[^particle-one] for that.
My colleague recommended me to make a small baby step changes,
test it out on the device making
sure it doesn't crash. [^1] 

[^particle-one]: For particle tracker one, you have to screw it open, and there is a mode and reset button, hold the mode button, then tap the reset button, once it starts flashing green release mode, this will allow you to put new firmeware on it again via usb.

[^1]: Which is starkly different from my haskell workflow
    of just writing everything you want in one go,
    solve a metric ton of compile errors and having
    a functioning program.
    This Made me realize how spoiled I am.

Anyway I got better,
the second week I got a build that sorted on signal strength values. [^struggles]
We believed the machine was sending random updates, 
whereas we were interested in strong signals mostly. [^requested]
So by sorting we'd just get the best information
out of the machine, neglecting weak signals.
This again showed an improvement, not as good 
as the increased packet size, 
but a good step forward.

[^struggles]: Apparantly you can use some c++ standard library function, trying to use a custom sort from the internet also cost me days. 😅

[^requested]: My colleague menioned they asked to sort the signals from the original contractors, but they never did this.

The following week I tackled the concurrency issue.
I knew this was an issue because there was a [mutex](https://stackoverflow.com/questions/34524/what-is-a-mutex)
that excluded the bleutooth scanner from
running while sending of packets was happening via the sim card (and vice versa).
In essence, they put stuff on a thread,
but nothing ran concurrently.
For example here is the scan thread doing the bleutooth scanning:
```c++
void Beaconscanner::scan_thread(void *param) {
    while(true) {
        if (!_instance->_run) {
            os_thread_yield();
            continue;
        }
        custom_scan_params();
        if(_instance->_scan_done) { // Wait until scan is consumed. Problem when loop is called at a lower frequency than scan period
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
    if (_scan_done) {
        publish_all();
        _scan_done = false;
    }
  }
```

You may wonder how these threads even communicate,
no handles are passed around.
The anwser is of course global variables and the use of various locking macros.[^globals] 
Not that the locking matters because `_scan_done` is used as a mutex.
So we've in fact two threads running,
but the relevant code never actually runs at
the same time.

> They wrote code, and then wrote other code to undo their initial code.

They took the effort of putting the bleutooth scanner on a thread.
And then wrote a mutex to undo the threading.
It would've all been more efficient if all was done on the main loop with no threading at all!
Once I realized this was happening I was baffled.
This guy got payed good money to undo his own code with more code.
This firmware was an especially sour lemon.

[^globals]: There was no reason for this and [it's](https://google.github.io/styleguide/cppguide.html#Static_and_Global_Variables) [suspicious](https://stackoverflow.com/a/485020).

I tried explaining this to my dear colleagues,
I said something like "we've threads but no concurrency".
Perhaps this was too dense,
but to be fair it's hard to explain nonsense.
Fortunatly this weirdness also visible in the observations. 
At times we were just getting no observations when you'd expect some.
So I said I had addressed this problem with the new build,
instead of trying to explain the technicalities.
I already had a build, because I knew the code was
nonsense and addressed it.

The fix involved deleting it all.
Both the locks and the mutex. 
Instead I used a queue for inter thread communication.
Queues is doing threads on easy mode,
and I had no-one to impress,
so I did easy mode.
However, even in easy mode this was still quite challenging to do.
Partly due to my in-experience with C-like programming.
So the queue I ended up using looked like [this](https://docs.particle.io/firmware/software-design/threading-explainer/?q=thread#os_queue_create):
```C
int os_queue_create(os_queue_t* queue, size_t item_size, size_t item_count, void* reserved);
```

It's a bit wacky, you reserve some memory
for the queue, giving you a `os_queue_t*`.
Then you've to inialize that memory with `os_queue_create`.
this fills in some fields of the structure.
The thing I got stuck on for a while was `os_queue_put` and `os_queue_take`
```C
int os_queue_put(os_queue_t queue, const void* item, system_tick_t delay, void* reserved);
int os_queue_take(os_queue_t queue, void* item, system_tick_t delay, void* reserved);
```
I think the issue was that I tried putting in a [`BleScanResult`](https://docs.particle.io/reference/device-os/api/bluetooth-le-ble/blepeerdevice/#blescanresult) directly.
But it contains some data which is pointers,
and this queue wants you to copy over all data as a continuous memory block.
`void*` in this case means give me whatever memory pointer, [^haskell-void]
and the `os_queue_create` function already told how large it should be.
So instead of putting everything on the queue, I just made a struct with
all relevant information:

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
I had to make some more modifications to support the primitives in the eddystone beacon class. [^class]
but once this was done it worked.

[^class]: with class we mean a c++ class, which is kindoff a module with implied mutable state. Or a struct, with member functions, it forces a construction function as well. 
[^know-why]: I think I only know this because I played around before with haskell to c FFI.

The results of these improvements are impressive:

| firmware          | p/s  |
|-------------------|------|
| lemons            | 1.4  |
| packets & sorting | 5.57 |
| unlocked          | 10.3 |

Here lemons is the original firmware, packets & sorting is after sorting.
And unlocked is me addressing the concurrency issues, by deleting the locks.
`p/s` stands for packets per second for strong signals in this case.
This is just a single measure for strong close signals bandwidth, 
but the new firmware improves in all categories we measured. [^force]
I did this in about 3 weeks of work.
Because we're going so fast there 
are more improvements in the pipeline because we've time,
such as using protobuf instead of JSON, which will increase troughput,
and tweaking bleutooth params causing a higher capture rate.
Which I may describe in a later post.
After we've done all that we can say we've lemonade!

[^force]: In the force update activity for examle, by just tapping the sensor to 
          a bleatooth tag we managed to increase success rate from 20% to about 90%. [^doesntwork]
          The system only managed to work without doing this by having long exposure to
          signals, but now we also support these quick exposures.
          Which makes the entire stack more robust.

Funny that I was quite unsure about even starting with the firmware,
or even suggesting it,
and I was even anxious about doing these concurrency updates.
They all turned out to have outsized impacts on the entire tech stack.
We're winning now.
This company may very well become a logistics company due to these calculated gambles.
Do you have any stories about professional calculated gambles?
Please let me know in the comments, I'd love to know 😀
