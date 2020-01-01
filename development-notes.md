# 1/1/2020
- Time to figure out how to remove the shutdown function from `SpecState`
- InterfaceSpec.hs is filled with notes. I have no idea if they are still relevant.
- So what I have now is
  ```haskell
    data SpecStateM m = SpecState
    { ssRunner   :: forall a. m a -> IO a
    , ssShutdown :: IO ()
    }
  ```

  but I think what I want is

  ```haskell
  newtype SpecStateM m = SpecStateM { unSpecStateM :: forall a. m a -> IO a }
  ```

  and I currently have `dbRunner` which is

  ```haskell
  dbRunner :: IO (SpecStateM DB)
  ```

  but I think it should be

  ```haskell
  withDbRunner :: (SpecStateM DB -> IO ()) -> IO ()
  ```

  I think this fine to change if I am only using `dbRunner` in the `beforeAll` of tests. I only see one use of `dbRunner` so this seems fine.

  Addressed in ffd3da3


# 12/31/2019

- CI is now compiling as much as I am locally. So time to get local compiling. I think I might have quit working on this when I hit a point where `with` would be useful in `tmp-postgres`.
- Scratch that getting an error from `PartialOptions` locally that do not get in CI.
- `postgresql-simple-opts >= 0.5.0.1` is busted. I was kinda of hoping I would not have to fix that right away. This is the problem and value of dog fooding I guess.
- Hopingfully I can modify `trek` to not depend on `postgresql-simple-opts >= 0.5.0.1`.
- I still can't believe I named the package "opts" instead of "options". I spelled out "postgresql-simple" but "options" was too much ... smh.
- I might be able to fix it by adjusting the dependencies.
- `therewebe` is using `postgresql-simple-opts >= 0.5.0.1`. I must of already fixed it and bumped the version.
- I fixed it for `postgres-options-0.1.0.1`. It is broken against 0.2.0.0. I just need to fix it.
- The readme I am copying around could be very wrong.
- Fixed the `postgresql-simple-opts` version issues. Now onto a `tmp-postgres` error.
- Commenting out Database.Trek.ToInterfaceImpl.hashConflicts because I don't remember what I was
  doing.
- The whole code is built around `SpecState` that is a db runner and shutdown function. Instead I should
  use `aroundAll` and the whole thing should just need a "with" interface of (DB a -> IO a).


# 12/30/2019

- The project doesn't compile and has uncommitted changes I don't understand. Going to commit them
  and add a travis config.
- Some packages have package.yaml's that are not used.
- I'm on some wierd branch ... that is going to be become master ... at least for now.
- Had to make repo public to get CI ... oh well.
- CI is failing on hspec-discover not being on the path ... twitter probably knows how to fix that.
- Someone on twitter had an answer: https://twitter.com/GeorgeTalksCode/status/1211915225999364096?s=20
