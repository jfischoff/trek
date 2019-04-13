module Database.Trek
   ( module Database.Trek.Types
   , module Database.Trek.Dump
   , module Database.Trek.Main
   , module Database.Trek.Db
   ) where
import Database.Trek.Types
import Database.Trek.Dump
import Database.Trek.Main
import Database.Trek.Db


{-
    How to determine that migrations do not need to be applied if a dump is used?
    If you use a dump then you should not apply those migrations
    but they are not in the migration table
    if you try to apply them they could fail
    the dump does not have the data you need
    Does it need the migration table dump as well?
    that would solve the problem
-}

{-
    The logic for a hash changing is to find the application it is part of
    and then remove all of the migrations that are associated with the application and any
    after it.

    it needs to store in the database that it should revert the db to a certain pitb at that point
-}







