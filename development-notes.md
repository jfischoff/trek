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
