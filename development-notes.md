#1/20/2020

- Adding back `worldState` and `clear`

#1/12/2020
- I think I am confusing myself. There is another interface lurking in the design but it is not necessary that I reify it
  with an actual interface file. Maybe I will one day but I don't need to now.

  The value of the current interfaces is they allow me to reuse tests at a low and high level. The interface I am thinking about that includes the `makeMigration :: Version -> String -> InputMigration` is nice because I replace the implementation
  but it is not about I think ... huh this interface will probably help me test too ... idk ... I don't feel like it doing it now.

- For now I need to finish the tests. I'm still unsure if I can easily add back the tests I had before.
- The test that the effects of migration succeed is why I used to have `worldState`. I don't think it will work though.
  If the world state includes sequences the output will be the not be the same if I rollback. I wonder if `clear`ing the
  schema would work? Probably.

  So what do I do? I could add back in `worldState` and `clear`. I think I should.

#1/11/2020
- If I change the interface of the "core" library to based on a String query I can't reuse it for some of the
  of the other ideas I have (jobs, etc)
- However I am not trying to do those other things now, so I am not sure it worth paying a complexity cost.
- Whether I use `String` in `InputMigration` or not, I need have way to use the interface that in involves a creating a migration from a string.
- I think there are two different concerns.
  - I want to be able to write tests for the migration interface that do not have to change if the implementation
    changes
  - I want to be able to have a interface for building a migrator that let's the implementation change.

  For testing I care less about the specifics of creation. For instance the implementation might have a name or not
  but that doesn't change the roundtrip property of the migrations.

  However whether it has a name of not changing what is need for creation so it is an important part of the interface for
  migrator.

  My current feeling is it is better to have two interfaces that perfectly fit the needs to how they are being used, then one but we'll see.

  I think this puts the question of whether to use a `DB ()` or a `String` in more focus. From the perspective of the `apply`
  property tests it doesn't matter.

  Choosing one or the another doesn't solve the problem that I would like to have a separate interface for actually making the migrator.

- I think for now there is an interface for building the migrator that requires a String. However I can still implement it with a `DB ()`.
- I think the question is whether I want to write a postgresql function that is used as the implementation for apply. I do want an adhoc way to add migrations with postgresql functions but the interface is different. It will only take in a single migration and derive the hash and version. I don't need a function for adding an array of migration inputs.
- So I think on final consideration nothing is gained really by moving to a `String` version.
- I can make the `applyMigrations` function faster with a `executeMany` but I don't think I should worry about that.

#1/10/2020
- I need to pass the input groups created_at in.
- Some thoughts on the created_at being client side. I get that it makes testing easier but
  it breaks the value of having a created_at column, record the order of the migrations were applied.
- I wonder if it faster to use the transaction id then timestampz.
- Either way I want to get rid of that restriction
- So this changes the tests. Instead of providing a way to make them equal, I'll provide my own equivalence function to compare them.
- Or I have to use libfaketime.
- Hmm ... on further thought I do like the client side created_at but I think there should also be an auto incremented sequence to order the migrations. The created_at is not to make an order but to give a rough sense of when it was created.
- I would like to rewrite this entirely in sql
- This becomes easier to test if the migrator is single threaded. Which it is at first but I am not sure if I should rely on it.
- No I don't think I should because that would make adding multithreaded behavior harder. Yes I should pass in a equivalence function. I can test that order is preserved on sequencial calls but not their exact numbers. So the order of the migrations hashes going in order of the hashes going out.
- So the order field doesn't show up.
- then I can use equals
- I'm debatting writing the insert as a single statement.
- It just makes a query that needs a lot of inputs. It increases the chance for a row tranposition error.
- Starting to wonder if I should use something other than postgresql simple. I'll stick with it for now.
- So should I make the big query? Yeah.
- It will probably let me remove some code.
- I'm starting to feel like I should just make the postgresql function interface first.
- I'll just write the tests with Haskell.
- This will also change the interface to take a `String` instead of a `DB ()`
- Which makes more sense and is simpler then adding a `fromString :: String -> DB ()`


#1/9/2020
- Modifying the InterfaceSpec.hs to not use setup and clear.
- I understand why I have clear and rollback. I need clear if the DB monad
  is not a transaction. This is to support enum alters for < 12 versions of postgres. I am fine with only supporting 12 to start out with if it makes things easier.
- I don't think Show and Eq necessarily need to be part of the Interface.hsig
- I'm not exactly sure how to modify the tests
- To write `toOutput` for the `DbTest.hs` I need to switch to using content addressable keys.

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

- Including the `inputGroup` in the interface was a way to ensure that `InputGroup` was non-empty ... but it can't do that anyway.
- There is nothing about the interface that points to it not handling an empty inputgroup. That is not something that core checks.
- I think what is needed for testing is `toOutput :: InputMigration -> OutputRecord` but I can't in general compute that. For instance if a rollback PiTR label is generated, or if anything is generated in the DB. I think preventing that from happening is too draconian.
- ~~The way the current tests must work is using `inputVersion   :: InputMigration -> Version` and `list` to verify versions are added currently but nothing else.~~ No `list` and `apply` both return `OutputGroup`s

- I think the properties for just apply are:
  - Empty migrations give Nothing

- Removing some notes from `InterfaceSpec.hs` to here for prosperity.

      the migrate filePath -> dispatches based type of file
      if it is a directory it tries to load each file
      -- it dispatches of the file type
      if it is a file it loads it as a newline manifest
      if it is a sql file it runs it and using the name to determine the migration
      if it is a sh it runs it

      Crazy idea. I can test the migrate filePath implementations with the same test interface

      Crazy idea more the exe can shelf test the extension scripts

      trek add-hashes [VERSION]

      trek remove-hashes [VERSION]

      Some ideas

      need to make the job runner next

      data Job = Job
        { batch :: LastRow -> DB [Row]
        , rowKey :: Row -> LastRow
        , action :: Row -> DB ()
        }

      And the coordinator that can take a migration and turn it into a sequence of
      migrations and jobs followed by code deployments.

      These might all be migrators

      The job system is sort of like a migration system that has a status
      The job system updates the migration until it is finished.

      The code deployment doesn't push something that is already out there

      Every thing is idepotent. It is basically a system for ensure idepotency

      but they are idepotent in different ways.
      The migration system does something or not.
      The job is incremental and updates state
      The code deployment is based on the output hash.

      Not clear how to have a single interface yet ... if at all.

      an interesting question is whether the system can handle concurrency. It should
      be able to

      A table lock sounds reasonable

      Some thoughts about the bigger picture. This is not just a migrator.
      It is a way to store actions to help achieve idempotency. The fact
      that it can do that easily for DB actions is a special case. Because
      we can lift into the DB (or perhaps it should be abstracted to a different
      monad) we can embed arbitrary IO.

      In this way the migrator can orchanstrate the steps to a zero down time
      deployment.

- One of the things I am noticing. The value of the current `list/apply` tests is I did not need to know how to convert an inputgroup to an outputgroup.

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
