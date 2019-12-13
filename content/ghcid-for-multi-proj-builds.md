TITLE: Ghcid for multi package projects
DATE: 2019-10-17 21:30
Modified: 2019-12-13 21:58
CATEGORY: tools
Tags: build-tools, haskell
OPTIONS: toc:nil

![GHCID magic](/images/2019/fire.svg)

When I tried [Ghcid](https://github.com/ndmitchell/ghcid)for a
[reflex](http://hackage.haskell.org/package/reflex) project,
it wouldn't rebuild on file change.
This is because reflex has a multi package project setup by default.
Recently I found that it is possible to use
[Ghcid](https://github.com/ndmitchell/Ghcid) for multi package project builds.
The trick is to create an executable that includes all the sources.
For example in [hpack](https://github.com/sol/hpack) style:

```yaml
tests:
  unit:
    main:                Spec.hs
    source-dirs:
    - test
    # including extra source dirs allows Ghcid to watch
    - src 
    - ../common/src
    - ../frontend/src
```

Now Ghcid will watch all the sources.
By targeting the test suite you can also immediately use this to run
unit tests.
If your other project use additional dependencies you may need to add
them to that test as well.

However because I'm doing reflex,
I had to compile JavaScript after Ghcid
typed checked everything.
I also had some unit tests to run.
Luckily Ghcid supports both a run and a test flag.
I simply used the `run` flag to run the tests and the `test`
flag to start GHCJS compilation.
Which brings us to this spell:

```bash
    ghcid -s "import Main" \
        -c "make update-cabal && cabal new-repl" \
        -T ":! cd .. && make after-native-build" \
        --run test:unit
```

The run flag can also be used to run your code if you don't have a weird
GHCJS requirement like me.
Running like that gives fast feedback because it's interpreted.

# Conclusion
This setup is an order of magnitude faster for debugging in my case.
If I have a compile error it now notifies me instantaneously,
whereas before it would take a second or two.
If I need to do a full build it takes about 10 seconds,
whereas before it was somewhere around a minute or two.
