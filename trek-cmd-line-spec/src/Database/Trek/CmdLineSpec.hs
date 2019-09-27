module Database.Trek.CmdLineSpec where
import Database.Trek.CmdLine
import Test.Hspec
import System.Exit

{-


> trek setup

> trek setup
exitcode: 8
stderr: Setup Already! >:(
-}

setupSpecs :: Spec
setupSpecs = do
  it "setup initially succeed" $ setup [] `shouldReturn` (ExitSuccess, [], [])


{-
> trek migrate FILEPATH
exitcode: 16
stderr: Not Setup! Execute `trek setup` to setup.
exitcode: 32
stderr: The following versions have hash conflicts [VERSION]
> trek migrate FILEPATH --warn-hash-conflicts
stderr: The following versions have hash conflicts [VERSION]
{ name           : foo
, version        : 12/12/1980
, hash           : xofdshagnosfdasngs
, rollback       : fdsqfg12
, application_id : 1
, created_at     : 12/12/2020
}
> trek migrate FILEPATH --no-warn-hash-conflicts
{ name           : foo
, version        : 12/12/1980
, hash           : xofdshagnosfdasngs
, rollback       : fdsqfg12
, application_id : 1
, created_at     : 12/12/2020
}
> trek migrate FILEPATH
{ rollback   : fdsqfg12
, id         : 1
, migrations :
  [ { name           : foo
    , version        : 12/12/1980
    , hash           : xofdshagnosfdasngs
    , application_id : 1
    , created_at     : 12/12/2020
    }
  , { name           : foo
    , version        : 12/12/1981
    , hash           : xofdshagnosfdasngs
    , application_id : 1
    , created_at     : 12/12/2021
    }
  ]
}
> trek migrate FILEPATH
exitcode: 64
stderr: Nothing to migrate!
> trek migrate FILEPATH --warn-empty-migration
stderr: Nothing to migrate!
> trek migrate FILEPATH --no-warn-empty-migration

trek hash-conflicts FILEPATH
exitcode: 16
stderr: Not Setup! Execute `trek setup` to setup.

trek add-hashes [VERSION]
exitcode: 16
stderr: Not Setup! Execute `trek setup` to setup.

trek remove-hashes [VERSION]
exitcode: 16
stderr: Not Setup! Execute `trek setup` to setup.

trek teardown
exitcode: 16

In the case that we don't care about hashes

trek migrate FILEPATH
trek list
trek teardown

the idea is that I can make a Impl out of this for the spec.
Also I can test that somethings are exactly as I expect them.

-- On failure it outputs the state to rollback to
trek migrate --error-on-conflict FILEPATH
trek list
trek teardown

-}



spec :: Spec
spec = do
  setupSpecs