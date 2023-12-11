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
    putStrLn $ show k
    userAccountMap <- atomically $ readTVar v
    let userAccountsList = Map.toList userAccountMap
    forM_ (userAccountsList) $ \(accountK, accountV) -> do
      amount <- atomically $ readTVar accountV
      putStrLn $ show accountK ++ " : " ++ show amount
	
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
deleteUser bank username = do
    usersMap <- readTVar (users bank)
    case Map.lookup username usersMap of
        Just userAccountsTVar -> do
            userAccounts <- readTVar userAccountsTVar
            case userAccounts == Map.empty of
              True -> do
                modifyTVar' (users bank) $ Map.delete username
              False -> do
                error "The user has accounts"
        Nothing -> do
          error "There is no such user"

openAccount :: Bank -> String -> Currency -> STM ()
openAccount bank username currency = do
    newAccount <- newTVar 0
    usersMap <- readTVar (users bank)
    case Map.lookup username usersMap of
        Just userAccountsTVar -> do
            userAccounts <- readTVar userAccountsTVar
            case Map.lookup currency userAccounts of
              Nothing -> do
                let changedAccount = Map.insert currency newAccount userAccounts 
                newAcc <- newTVar changedAccount 
                modifyTVar' (users bank) $ Map.insert username newAcc
              Just _ -> do
                modifyTVar' (users bank) $ Map.insert username userAccountsTVar
        Nothing -> do
          error "There is no such user"

closeAccount :: Bank -> String -> Currency -> STM ()
closeAccount bank username currency = do
    usersMap <- readTVar (users bank)
    case Map.lookup username usersMap of
        Just userAccountsTVar -> do
            userAccounts <- readTVar userAccountsTVar
            case Map.lookup currency userAccounts of
              Nothing -> do
                let changedAccount = Map.delete currency userAccounts 
                newAcc <- newTVar changedAccount 
                modifyTVar' (users bank) $ Map.insert username newAcc
              Just amountTVar -> do 
                let changedAccount = Map.delete currency userAccounts 
                newAcc <- newTVar changedAccount 
                amount <- readTVar amountTVar
                if amount == 0
                  then modifyTVar' (users bank) $ Map.insert username newAcc
                  else error "The amount is not zero"
        Nothing -> do
          error "There is no such user"

addAmount :: Bank -> String -> Currency -> Int -> STM ()
addAmount bank username currency value = do
    usersMap <- readTVar (users bank)
    case Map.lookup username usersMap of
        Just userAccountsTVar -> do
            userAccounts <- readTVar userAccountsTVar
            case Map.lookup currency userAccounts of
              Nothing -> do
                error "There is no account"
              Just amountTVar -> do
                amount <- readTVar amountTVar
                newAccount <- newTVar (amount + value)
                let changedAccount = Map.insert currency newAccount userAccounts 
                newAcc <- newTVar changedAccount 
                modifyTVar' (users bank) $ Map.insert username newAcc
        Nothing -> do
          error "There is no such user"
		  
withdrawMoney :: Bank -> String -> Currency -> Int -> STM ()
withdrawMoney bank username currency value = do
    usersMap <- readTVar (users bank)
    case Map.lookup username usersMap of
        Just userAccountsTVar -> do
            userAccounts <- readTVar userAccountsTVar
            case Map.lookup currency userAccounts of
              Nothing -> do
                error "There is no account"
              Just amountTVar -> do
                amount <- readTVar amountTVar
                if (amount < value) 
                  then error "Insufficient funds"
                  else do
                    newAccount <- newTVar (amount - value)
                    let changedAccount = Map.insert currency newAccount userAccounts 
                    newAcc <- newTVar changedAccount 
                    modifyTVar' (users bank) $ Map.insert username newAcc
        Nothing -> do
          error "There is no such user"

{-transferBetweenAccounts :: Bank -> String -> Currency -> Currency -> Int -> STM ()
transferBetweenAccounts bank username fromCurrency toCurrency amount = do
    usersMap <- readTVar (users bank)
    case Map.lookup username usersMap of
        Just userAccountsTVar -> do
            userAccounts <- readTVar userAccountsTVar
            case Map.lookup fromCurrency userAccounts of
              Nothing -> do
                error "There is no account with " ++ fromCurrency
              Just amountTVarFromCurrency -> do
                amountFromCurrency <- readTVar amountTVarFromCurrency
                if (amountFromCurrency < value) 
                  then error "Insufficient funds in " ++ fromCurrency
                  else do
                    case Map.lookup toCurrency userAccounts of
                      Nothing -> do
                        error "There is no account with " ++ toCurrency
                      Just amountTVarToCurrency -> do
                        amountToCurrency <- readTVar amountTVarToCurrency
                        newAccountFromCurrency <- newTVar (amountFromCurrency - value)
                        newAccountToCurrency <- newTVar (amountToCurrency + value)
                        let changedAccountFromCurrency = Map.insert currency newAccountFromCurrency userAccounts 
                        let changedAccountToCurrency = Map.insert currency newAccountToCurrency userAccounts 
                        newAccFromCurrency <- newTVar changedAccountFromCurrency 
                        newAccToCurrency <- newTVar changedAccountToCurrency 
                        modifyTVar' (users bank) $ Map.insert username newAccFromCurrency
                        modifyTVar' (users bank) $ Map.insert username newAccToCurrency
        Nothing -> do
          error "There is no such user"-}

transferBetweenAccounts :: Bank -> String -> Currency -> Currency -> Int -> STM ()
transferBetweenAccounts bank username fromCurrency toCurrency amount = do
    withdrawMoney bank username fromCurrency amount
    addAmount bank username toCurrency amount

transferBetweenUsers :: Bank -> String -> String -> Currency -> Currency -> Int -> STM ()
transferBetweenUsers bank fromUser toUser fromCurrency toCurrency amount = do
    withdrawMoney bank fromUser fromCurrency amount
    addAmount bank toUser toCurrency amount

someFunc :: IO ()
someFunc = do
    bank <- atomically initializeBank
    atomically $ do
        registerUser bank "User1"
        openAccount bank "User1" "RUB"
        addAmount bank "User1" "RUB" 12
        registerUser bank "User2"
        openAccount bank "User2" "USD"
        transferBetweenUsers bank "User1" "User2" "RUB" "USD" 6
    printBank bank
