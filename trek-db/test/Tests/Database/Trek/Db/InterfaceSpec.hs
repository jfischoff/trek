module Tests.Database.Trek.Db.InterfaceSpec where

import Tests.Database.Trek.Db.Interface

{-
all with a clean schema

setup                 -> DidNotHaveSetup
setup    >> setup     -> HadSetup

teardown             -> DidNotHaveSetup
teardown >> teardown -> DidNotHaveSetup

teardown >> setup    -> DidNotHaveSetup
setup    >> teardown -> HadSetup

-}

setupTeardownSpecs
  :: DB SetupResult
  -> DB SetupResult
  -> Spec
setupTeardownSpecs setup teardown = describe "setup teardown" $ do
  let beforeActions =
        [ ("nothing" , pure () )
        , ("setup"   , setup   )
        , ("teardown", teardown)
        ]
  forM beforeActions $ \(actionName, beforeAction) -> describe ("before " ++ actionName) $ do
    it "setup succeeds on a clean schema" $ withClearSchema $
      setup `shouldReturn` DidNotHaveSetup
    it "teardown then setup succeeds" $ withClearSchema $
      (beforeAction >> teardown >> setup) `shouldReturn` DidNotHaveSetup
    it "setup projection" $ withClearSchema $
      (beforeAction >> setup >> setup) `shouldReturn` HadSetup
    it "setup then teardown gives" $ withClearSchema $
      (beforeAction >> setup >> teardown) `shouldReturn` HadSetup
    it "teardown fails on clean schema" $ withClearSchema $
      teardown `shouldReturn` DidNotHaveSetup
    it "teardown projection" $ withClearSchema $
      (beforeAction >> teardown >> teardown) `shouldReturn` DidNotHaveSetup

{-
assuming: cleanSchema >> setup >> runExcept

>> wraps in ExceptT

listMigrations = pure []

migrate (a, xs) >> listMigrations = pure [(a, xs)]
migrate xs >> forall subset xs. migrate subset >> Except listMigrations = pure xs
migrate xs >> forall subset xs. migrate (subset `union` y) >> listMigrations = pure (xs `union` y)
-}

migrateSpecs :: Interface -> NonEmpty (Migration, DB Bool) -> Spec
migrateSpecs Interface {..} migrations = describe "migration" $ do
  let expectNoSetup action = do
    it "for migrate" $ withCleanSchema $ do
      action
      iMigrate Aeson.Null (fmap fst migrations) `shouldReturn` Left NoSetup
    it "for getApplicationGroup" $ withCleanSchema $ do
      action
      iGetApplicationGroup (ApplcationId 0) `shouldReturn` Left NoSetup
    it "for listMigrations" $ withCleanSchema $ do
      action
      iListMigrations `shouldReturn` Left NoSetup
    it "for hashConflicts with nonempty migrations" $ withCleanSchema $ do
      action
      iHashConflicts (toList $ fmap fst migrations) `shouldReturn` Left NoSetup

  describe "Clean schema gives NoSetup" $ expectNoSetup (pure ())
  describe "setup >> teardown gives NoSetup" $ expectNoSetup (setup >> teardown)

  it "listMigrations on clean setup gives []" $ withSetup $
    iListMigrations `shouldReturn` []

  it "a single migration on a clean setup succeeds" $ withSetup $
    let (theMigration, validator) = NonEmpty.head migrations
    Right (Just appliedMigrations) <- iMigrate Aeson.Null (pure theMigration)
    appliedMigration `shouldBe` ApplicationGroup Aeson.Null $ pure theMigration

    True <- validator

    fmap clearCreatedAt iListMigrations `shouldReturn` Right [clearCreateAt appliedMigration]

  let (theMigrations, validations) = unzip migrations
      validateAll = mconcat $ fmap (fmap All) $ validations

  let verifyUnionWithSubset new = do
        Right (Just appliedMigrations) <- iMigrate Aeson.Null theMigrations
        appliedMigrations `shouldReturn` ApplicationGroup Aeson.Null theMigrations

        True <- validateAll

        let (theExtraMigrations, theExtraValidations) = unzip extraMigrations
            extraValidateAll = mconcat $ fmap (fmap All) $ validations

        forSubsetsOf theMigrations $ \subset -> rollback $ do
          iMigrate Aeson.Null (subset <> new)`shouldReturn` Right Nothing
          iListMigrations `shouldReturn` appliedMigrations:[ApplicationGroup Aeson.Null new]

  it "subsets of the already applied migrations return Nothing and listMigrations returns the same as before" $ withSetup $
    verifyUnionWithSubset []

  it "subset with union of a new versions return the original plus the new versions" $ withSetup $
    verifyUnionWithSubset (toList extraMigrations)

hashConflictSpecs hashConflicts = do
  it "no hash conflicts on empty" $ withAllMigrated $
    iHashConflicts (toList conflictingMigrations) `shouldReturn` Right []
  it "any subset of the conflictingMigrations union with extraMigrations and the conflicts are only the subset" $ withAllMigrated $
    forSubsetsOf conflictingMigrations $ \subset ->
      iHashConflicts (subset ++ map fst (toList extraMigrations)) `shouldReturn` Right [subset]
      iHashConflicts subset `shouldReturn` Right [subset]

{-
  Right (Just appliedMigrations) <- iMigrate Aeson.Null theMigrations
  appliedMigrations `shouldReturn` ApplicationGroup Aeson.Null theMigrations
-}


specs :: Interface -> Spec
specs Interface {..} = describe "Tests.Database.Trek.Db.Interface" $ do
  setupTeardownSpecs iSetup iTeardown
  migrateSpecs iMigrate iGetApplicationGroup iListMigrations
  hashConflictSpecs iHashConflicts
