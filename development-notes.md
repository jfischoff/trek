#1/6/2020
- I am not sure if `inputGroup` will work for cmd line interface.
- The `InputMigration` is probably a query string and some meta info. However the `InputGroup` is a directory path. Creating an `InputGroup` from a `InputMigration`s is an IO operation so I need the signature to be `inputGroup :: NonEmpty InputMigration -> DB InputGroup`
- I don't see where `inputGroup` is used outside of the tests so it should probably be moved out of the interface as well.
- I can remove the `Version` from the interface.
- I am not a fan of the `inputAction`. I'm not a fan of having a separate interface so I can test things.
- I am surprised I can't make a single interface that can be used for building the libraries and the tests.
- I'm not sure testing an extended interface means you have tested the interface.

- I am starting to think I need to modify the TestInterface.hsig.
  - `clear` seems unnecessary. `rollback` should be enough.

- I think I am going to simplify the interface and then see if I can rewrite the tests without the test interface.

# 1/5/2020
- I've decided that the extra work of writing adapters to maintain the more complex db core is not the fastest path. I should remove the functionality and only maintain a db core that has the interface the cmd line interface supports.
- However before I start the process of removing the features, I should get what I currently have to compile in some sense.
- Trying to get CI working.
- My problem with private libraries and --dep is affecting someone else: https://github.com/haskell/cabal/issues/6081
- I'm trying to call --dep on the packages in a order that might work ... idk
  - cabal v2-build --enable-tests --dep trek-db-interface
  - cabal v2-build --enable-tests --dep trek
  - cabal v2-build --dep trek-db-spec (I am now wondering if I can install the deps for without the tests first)
  - cabal v2-build --dep all (this is working now ... not sure if it would work from a clean. Need to check.)
  - Even after install the non test deps I can't install the test deps.
  - I think I am going to have to manually include test deps in the non-test parts.
- Remove CI cache and pray.
- The `trek-db-spec` has a reference implementation that is pure that is used to test the spec. I wanted to do the same thing with `trek-cmd-line-spec` but I don't think I can make a pure implementation because it needs to connect to the db. That is part of what I am testing I think. That it can parse the environment variables and connect.
- I think I could make a reference implementation using IORefs that doesn't connect to a the db. I don't see all the pieces right now and I am not sure if it is worth it.
- I need to list out the tests for `apply` and `create`
- If I am going to test that the hashes are something specific I need to specify the way the application `hash` is
  calculated. I could just concat the hashes and hash the result. However then I would have an identical hash for the application and migration which would make them not globally unique. However if I append "application|" to the hash I could maintain global uniqueness probably. I'm going to do the former but I have to document it either way.

# 1/4/2020
- Here is my current way I would describe this project.
  - There is a core library that is built around the functions: `setup`, `teardown`, `apply` and `list`.
  - The MVP command line will have `apply` and `create`.
  - The core library doesn't have to a way to parse queries currently but I might need a way to create a query from a
    filepath: `FilePath -> IO (DB ())` (or should it be `FilePath -> IO (DB a)`?).
  - We need to make an autosetup interface for just `apply`. The properties that the `setup` necessary `apply` must     statisfy if the db has been `setup` are the properties that auto-setup `apply` must have. The `setup` necessary     `apply` must pass a superset of the auto-setup `apply`'s properties. I'm not being very clear.
  - I'm not sure if I will ever expose the more complex interface or not. But it is also an implementation for the
    auto-setup interface.
  - That is all the core autosetup apply interface must specify. And there must be a way to adapt the `apply :: IO ()`
    to reuse the tests.
  - If a core test fails against the executable we can rerun on different levels to find where it is breaking.

- Things I need to do
  - Make the autosetup `apply` only hsig (and project!).
  - Make the `apply` with `setup` adapter.
  - Refactor the `apply` properties to the autosetup Spec and have the with `setup` Spec depend on it.
  - Make the new cmdline hsig.
  - Make the `apply :: IO ()` adapter for the core properties.
  - Finish the tests for cmd line interface.

# 1/3/2020
- I question whether I can use the core tests for the MVP interface.
- The self setup `apply` has fewer tests cases. I think it is a subset of the `apply` with `setup`.
- Every `apply` with `setup` can be used to instaniate a `apply` with auto-setup interface.
- I don't like the `CmdLine.hsig`. For one thing it assumes it gets a list of command line arguments but no
  environment variables. It just seems unnecessary.
- I think `apply :: IO ()` is probably the right interface. I can hopefully catch the `ExitFailure`s.
- I will need to capture its standard handles when testing.
- `apply :: IO ()` with be built with an autosetup db `apply`.
- So it `apply :: IO ()`:
  - read the environment variables and make a data connection.
  - read the migration from a file. catch and error and exit.
  - parse the migration file name. catch and error and exit.
  - run the autosetup apply. catch and error and exit.
  - print result
- I should fix this interface and get things to compile.

# 1/2/2020
- I can simplify `CmdLineSpec.hs`. I can remove all of the `listApplications` stuff.
- I should remove it all together. Better to make things simplier while I am still figuring out the interface.
  I can always bring it back.
- I don't get this `apply :: [FilePath]` function

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
- Everything is compiling but the tests are failing with a bunch of `undefine`s causing exceptions.
- There are few undefines but the one that is failing is:
  ```haskell
  makeTestMigrations :: FilePath -> TestMigrations
  makeTestMigrations = undefined
  ```

  I'm starting to remember what this file is testing. It is testing that the command line interface works as expected.
  For the command line version the migrations are files in a directory. I think the idea with `makeTestMigrations` is
  it is given a temporary directory and makes three migrations:

  ```haskell
  data TestMigrations = TestMigrations
    { successfulMigration  :: FilePath
    , migrationDirectory   :: FilePath
    , extraMigration       :: FilePath
   }
  ```

  The `migrationDirectory` is probably what is passed in to `makeTestMigrations` so I clean it up?

  The `successfulMigration` I think I get. Actually I'm not sure. It could be a migration that is already applied or one that should be successful. I can't remember what the `extraMigration` is used for but it is common pattern in the code base so I should be able to figure it out.

- `extraMigration` and `successfulMigration` are just two migrations to apply that should succeed. However I am already testing that their application results in specific records. I feel like I should look to see if the migration results can be copied.
- There are lot of notes in the code about the need for "parsers". I think it has something to do with reusing the core tests on the cmd line interface. I hope I have already written these parsers.
- I think the things like `successfulMigrationRecord`

  ```haskell
  successfulMigrationRecord :: String
  successfulMigrationRecord = [here|
  { "rollback"   : fdsqfg12
  , "id"         : 1
  , "migrations" :
    [ { "name"           : "foo"
      , "version"        : "12/12/1980"
      , "hash"           : "xofdshagnosfdasngs"
      , "rollback"       : "fdsqfg12"
      , "application_id" : 1
      , "created_at"     : "12/12/2020"
      }
    ]
  }
  |]
  ```

  Were aspirational. I don't have anything that can produce this output.

- I'm starting to remember where this project is at. I am trying to finish writing the tests for the cmd line interface. I also have to write adapters to the tests.
- Something I don't get about `applyListApplicationsSpecs` is I think it is duplicate of the stuff in the core tests ... which is what I was trying to avoid. Need to verify.
- I need to make a diagram of how all the packages relate. I can't remember everything.
- `apply :: InputGroup -> DB (Maybe (Maybe OutputGroup))` is a little ugly with the `Maybe (Maybe ... )`. Maybe
   `Setup (Maybe ... )`
- I think I understand the difference between the core tests and the tests in CmdLineSpec.hs. The tests in a CmdLineSpec are mostly testing exit code and warning messages.
- I'm not sure that all the tests are necessary. Oh wait I think I get it. The "migrates more then one in a group" test
  is just to make sure that the output for a list of migrations is what I expect it to be. Yeah I return a single migration if the list is one for some reason and only a list if there are two. This is what I am trying to test.
- I would need to parse the output to do the other tests.
- Parsing should be okay because this is all json.
- I'm missing a lot of tests. Only did `list/apply`
- Actually that might be all for the simplest version of the migrator ...
- I think to make a test migration I need to encode the version and name in file path.
- I think the hash should probably be the id. Hardcoded auto-incremented ids in tests is a
  great way to make them fragile. If the ids are content addressable I think it will make everything
  more reliable.
- The `created_at` looks hard to test as well. I think I should probably be sending it in anyway.
- I'm not sure I understand why I have `rollback` in there. I assume that is the PiTR key. I don't think I have implemented that A., B. I don't think my first version should worry about that.

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
