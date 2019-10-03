{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables #-}
import Test.Hspec
import Test.QuickCheck
import Control.Exception(evaluate)
import Control.DeepSeq
import System.Exit (ExitCode(..))
import Database.Persist(Entity(..),deleteWhere)
import qualified Commands.Index as I
import qualified Lib as Lib
import Database2
import Data.Text
import System.Environment(setEnv)
import Control.Monad.IO.Class (liftIO)
import qualified Database.Persist.Postgresql as Pgsql
import qualified Database.Persist.Class as PClass

main :: IO ()
main = hspec $ before_ selectTestDB $do
  describe "I.getUser" $ do
    it "returns the user if the user exists" $do
      pool <- getDbPool
      flip Pgsql.runSqlPersistMPool pool $ do
        _ <- PClass.insertUniqueEntity User {userEmail = "test@test.com", userUsername = "test",userPassword = "test", userSessionId = Nothing}
        deleteWhere [UserUsername Pgsql.==. "nonexistinguser"]
        pure()
      
      user <- I.getUser "test"
      let u = getv user
      userUsername u `shouldBe` "test"
    context "if the user doesn't exist" $ do
      it "throws an exception" $ do
        I.getUser "nonexistinguser" `shouldThrow` anyErrorCall
        
  describe "I.getList" $ do
    it "returns the list if the list exists" $do
      pool <- getDbPool
      user <- I.getUser "test"
      flip Pgsql.runSqlPersistMPool pool $ do
        _ <- PClass.insertUniqueEntity List { listName = "testlist", listUserId = (entityKey user), listOrder = 0 }
        deleteWhere [ListName Pgsql.==. "nonexistinglist"]
        return ()
      
      
      list <- I.getList "testlist" user
      let l = getv list
      listName l `shouldBe` "testlist"
      listOrder l `shouldBe` 0
    context "if the list doesn't exist" $ do
      it "throws an exception" $ do
        user <- I.getUser "test"
        
        I.getList "nonexistinglist" user `shouldThrow` anyErrorCall

  describe "I.getItems" $ do
    it "returns the items belonging to a user" $do
      pool <- getDbPool
      user <- I.getUser "test"
      flip Pgsql.runSqlPersistMPool pool $ do
        _ <- PClass.insertUniqueEntity Item { itemName = "testitem", itemUserId = (entityKey user)}
        return ()
     
      items <- I.getItems user
      let itemvals = Prelude.map getv items
      let itemnames = Prelude.map itemName itemvals
      items `shouldSatisfy` (not . Prelude.null)
      itemnames `shouldSatisfy` (Prelude.any (== "testitem"))
      
  describe "I.getListItems" $ do
    it "returns the items in a list" $do
      pool <- getDbPool
      user <- I.getUser "test"
      list <- I.getList "testlist" user
      items <- I.getItems user
      
      flip Pgsql.runSqlPersistMPool pool $ do
        _ <- PClass.insertUniqueEntity Item { itemName = "testitem", itemUserId = (entityKey user)}
        return ()
      let testitem = Prelude.head $ Prelude.filter (\x-> getname x == "testitem") items
            where getname (Entity k v) = itemName v
      I.addItemToList testitem list Nothing
      listitems <- I.getListItems list
            
      listitems `shouldSatisfy` (not . Prelude.null)


  describe "I.getListMixinByChild" $ do
    it "returns the parent lists of a child list" $do
      pool <- getDbPool
      user <- I.getUser "test"
      list <- I.getList "testlist" user
      
    
      flip Pgsql.runSqlPersistMPool pool $ do
        _ <- PClass.insertUniqueEntity List { listName = "testchildlist", listUserId = (entityKey user), listOrder = 1 }
        return ()
        
      childlist <- I.getList "testchildlist" user
      
      flip Pgsql.runSqlPersistMPool pool $ do
        _ <- PClass.insertUniqueEntity ListMixin { listMixinParent = (entityKey list), listMixinChild = (entityKey childlist), listMixinOrder = 0}
        return ()
        
      
      mixin <- I.getListMixinByChild childlist
      let mixinvals = Prelude.map getv mixin
      let listparent = Prelude.map listMixinParent mixinvals
      
      listparent `shouldSatisfy` (Prelude.any (== entityKey list))

      
  describe "I.getListMixinByParent" $ do
    it "returns the child lists of a parent list" $do
      pool <- getDbPool
      user <- I.getUser "test"
      list <- I.getList "testlist" user
      
    
      flip Pgsql.runSqlPersistMPool pool $ do
        _ <- PClass.insertUniqueEntity List { listName = "testchildlist", listUserId = (entityKey user), listOrder = 1 }
        return ()
        
      childlist <- I.getList "testchildlist" user
      
      flip Pgsql.runSqlPersistMPool pool $ do
        _ <- PClass.insertUniqueEntity ListMixin { listMixinParent = (entityKey list), listMixinChild = (entityKey childlist), listMixinOrder = 0}
        return ()
        
     
      mixin <- I.getListMixinByParent list
      let mixinvals = Prelude.map getv mixin
      let listchild = Prelude.map listMixinChild mixinvals
      
      listchild `shouldSatisfy` (Prelude.any (== entityKey childlist))
      
   
  
selectTestDB = do
  setEnv "env" "test"
  
  dbMigration
  putStrLn "done migration to test DB"

dbMigration :: IO ()
dbMigration  = do
  pool <- Database2.getDbPool
  flip Pgsql.runSqlPersistMPool pool $ do
    
    Pgsql.runMigration Database2.migrateDatabase

getv :: Entity record -> record
getv (Entity k v) = v
