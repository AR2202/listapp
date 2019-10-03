module NonGUI.Interface 
  (interface
  )
  where
import System.Environment   
import System.IO
import Data.List
import Database.Persist(Entity(..),get)
import qualified Lib as Lib
import Database2
import Data.Char (toLower, toUpper)
import qualified Commands.Index as I
import Data.Text(Text, pack,unpack)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust)
  

--listopt :: [(String, [String] -> IO ())]  
listopt =  [ ("0", newList)  
            , ("1", findList)  
            
            ]  
listitemopt = [ ("0", addItemToList)  
            , ("1", deleteItemFromAList)
            , ("2", saveList)
            , ("3", chooseList)
            , ("4", exit)
            
            ]  
            
interface = do
  putStrLn "Sign in or Register?"
  signinopt <- getLine
  case (map toLower signinopt) of
    "sign in" ->  do
      putStrLn "username:"
      username <- getLine
      putStrLn "password:"
      password <- getLine
      user <- I.getUser username
      let v = (userPassword$ getv user)
      if v == pack password
   
        then startSession username user
        else do
         putStrLn "invalid passoword"
         interface
      
    "register" ->do
      putStrLn "username:"
      username <- getLine
      putStrLn "passoword:"
      password <- getLine
      putStrLn "New user"
    _-> do
      putStrLn "Not a valid option"
      interface
    
  
    



getv :: Entity record -> record
getv (Entity k v) = v



getk (Entity k v) = k


startSession :: String -> Entity User -> IO()
startSession username eUser = do
  putStrLn$ "Hello " ++ username
  putStrLn "What would you like to do?"
  let options = ["Create a new list", "Find an existing list"]
  let numbered = zipWith (\n line -> show n ++ " - " ++ line) [0..] options
  putStr$ unlines numbered
  option <- getLine
  let selection = lookup option listopt
  case selection of
    Nothing -> do
      putStrLn "Not a valid selection"
      startSession username eUser
    Just task -> task eUser 


newList :: Entity User -> IO()      
newList eUser = do
  putStrLn "Enter list name"
  listname <-getLine
  pool <- getDbPool
  I.createList' pool eUser listname
  selectList listname eUser

findList :: Entity User -> IO()
findList eUser = do
  putStrLn "Your Lists:"
  pool <- getDbPool
  allLists <- I.getLists' pool eUser
  let listnames = map getname allLists
        where getname (Entity k v) = listName v
  let numberedlistnames = zipWith (\n line -> show n ++ " - " ++ unpack line) [0..] listnames
  let listdict = zip (map show [0..]) (map unpack listnames)
  mapM putStrLn numberedlistnames
  putStrLn "choose list (enter number):"
  option <- getLine
  let selection = lookup option listdict
  case selection of
    Nothing -> do
      putStrLn "Not a valid option"
      findList eUser
    Just sel -> selectList sel eUser
      

      


selectList listname eUser = do
  putStrLn listname
  pool <- getDbPool
  list <- I.getList' pool eUser listname
  changeList listname list eUser
  

changeList listname eList eUser = do
  listitems <- I.getListItems eList
  let items = map I.getItemFromListItem listitems
  let getitemname (Entity k v) = itemName v
  putStrLn $ listname ++ " has the following items:"
  mapM_ (\i -> i>>= putStrLn.unpack.getitemname) items
  
  putStrLn "What would you like to do"
  let options = ["Add an item to the list", "Delete an item", "save the list", "choose a different list","exit"]
  let numbered = zipWith (\n line -> show n ++ " - " ++ line) [0..] options
  putStr$ unlines numbered
  option <- getLine
  let selection = lookup option listitemopt
  case selection of
    Nothing -> do
      putStrLn "Not a valid selection"
      changeList listname eList eUser
    Just task -> task listname eList eUser
     
addItemToList listname eList eUser = do
  pool <- getDbPool
  putStrLn "Which item do you want to add?"
  itemtoadd <- getLine
  print itemtoadd
  maybeitem <- I.getItem' pool eUser itemtoadd
  case maybeitem of
    Nothing -> do
      _<-I.addItem' pool eUser itemtoadd
      justitem <-I.getItem' pool eUser itemtoadd
      I.addItemToList (fromJust justitem) eList Nothing
      changeList listname eList eUser
    Just item -> do
      I.addItemToList item eList Nothing
      changeList listname eList eUser

deleteItemFromAList listname eList eUser = do
  putStrLn "Which item do you want to delete?"
  pool <- getDbPool
  itemtodelete <- getLine
  print itemtodelete
  maybeitem <- I.getItem' pool eUser itemtodelete
  case maybeitem of
    Nothing -> do
      putStrLn "Item does not exist"
      changeList listname eList eUser
    Just item -> do
      I.deleteItemFromList item eList
      changeList listname eList eUser

saveList listname eList eUser = do
  putStrLn "Enter filepath"
  path <- getLine
  let filepath = path ++ listname ++".tex"
  
  toTex listname eList filepath
  

itemize :: String -> String
itemize item = "\\item "++item


toTex listname eList filepath = do
  listitems <- I.getListItems eList
  let items = map I.getItemFromListItem listitems
  let getitemname (Entity k v) = itemName v
  handle <- openFile filepath WriteMode
  hPutStrLn handle "\\documentclass [a4paper, 12pt]{article}"
  hPutStrLn handle $ "\\title{"++listname++"}"
  hPutStrLn handle "\\date{}"
  hPutStrLn handle "\\begin {document}"
  hPutStrLn handle "\\maketitle"
  hPutStrLn handle "\\begin {itemize}"
  let writetohandle = hPutStrLn handle
  mapM_ (\i -> i>>= writetohandle.itemize.unpack.getitemname) items
  hPutStrLn handle "\\end {itemize}"
  hPutStrLn handle "\\end{document}"
  hClose handle

chooseList listname eList eUser = findList eUser
  

exit listname eList eUser = do
  putStrLn  "goodbye" 
