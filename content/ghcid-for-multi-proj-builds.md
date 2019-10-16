TITLE: GHCID for multi project builds
DATE: 2019-09-06
CATEGORY: tools
Tags: haskell build-tools
OPTIONS: toc:nil
status: draft

It's possible to use ghcid for multiproject builds.

The trick is to create an executable or test that includes all the sources.
For example asuming a reflex style project:
```yaml
tests:
  unit:
    main:                Spec.hs
    source-dirs:
    - test
    # including extra source dirs allows ghcid to watch
    - src 
    - ../common/src
    - ../frontend/src
```
Now this can be used as target for ghcid.
By using a unit test you can also immediatly use this to run
your unit tests.

In my case I had to compile javascript after ghcid gave the okay,
and run the tests.
Luckily ghcid supports both a run and test command.
I simply used run to run the tests and test to start javascript.
I ended up with this spell:

```make
ghcid:
	nix-shell --run "ghcid -s \"import Main\" -c \"make update-cabal && cabal new-repl\" -T \":! cd .. && make after-native-build\" --run test:unit"
```

It can also run your code after finishing building,
which is interpreted.

This is an order of magnitude faster for debugging in my case.
If I have a compile error it now notifies me instantatiously,
whereas before it would take a second or two.
And if I need to do a full build it takes about
10 seconds, whereas before it was somewhwere around a minute
or two.

