module MyLib (someFunc) where

import Control.Concurrent.STM
import qualified Data.Map as Map
import Control.Monad	

type Currency = String
type Amount = Int
type Account = TVar Amount
type UserAccounts = TVar (Map.Map Currency Account)
type BankAccount = TVar (Map.Map Currency Amount)

data Bank = Bank
    { users :: TVar (Map.Map String UserAccounts)
    , bankAccount :: BankAccount
    }

printBank :: Bank -> IO ()
printBank bank = do
  usersMap <- atomically $ readTVar (users bank)
  bankAccountMap <- atomically $ readTVar (bankAccount bank)
  let usersList = Map.toList usersMap
  let bankAccountList = Map.toList bankAccountMap
  forM_ (usersList) $ \(k,v) -> do
    putStrLn $ show k ++ ": $ "
	
initializeBank :: STM Bank
initializeBank = do
    usersVar <- newTVar Map.empty
    bankAccountVar <- newTVar Map.empty
    return $ Bank usersVar bankAccountVar

registerUser :: Bank -> String -> STM ()
registerUser bank username = do
    newAccounts <- newTVar Map.empty
    modifyTVar' (users bank) $ Map.insertWith (const id) username newAccounts

deleteUser :: Bank -> String -> STM ()
deleteUser bank username =
    modifyTVar' (users bank) $ Map.delete username


someFunc :: IO ()
someFunc = do
    bank <- atomically initializeBank
    atomically $ do
        registerUser bank "User1"
        deleteUser bank "User1"
    printBank bank
