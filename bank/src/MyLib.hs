module MyLib where

import Control.Concurrent.STM
import qualified Data.Map as Map
import Control.Monad	

type Currency = String
type Amount = Double
type Account = TVar Amount
type Accounts = TVar (Map.Map Currency Account)

bankCurrency = "USD"

data Bank = Bank
    { users :: TVar (Map.Map String Accounts)
    , bankAccount :: Accounts
    , comission :: TVar Double
    }

printBank :: Bank -> IO ()
printBank bank = do
  usersMap <- atomically $ readTVar (users bank)
  bankAccountMap <- atomically $ readTVar (bankAccount bank)
  let usersList = Map.toList usersMap
  let bankAccount = Map.toList bankAccountMap
  putStrLn "Users:"
  forM_ (usersList) $ \(k,v) -> do
    putStrLn $ show k
    userAccountMap <- atomically $ readTVar v
    let userAccountsList = Map.toList userAccountMap
    forM_ (userAccountsList) $ \(accountK, accountV) -> do
      amount <- atomically $ readTVar accountV
      putStrLn $ show accountK ++ " : " ++ show amount
  putStrLn "Bank account:"
  forM_ (bankAccount) $ \(k,v) -> do
    putStr $ show k ++ " : "
    bankAccountAmount <- atomically $ readTVar v
    putStrLn $ show bankAccountAmount
	
initializeBank :: STM Bank
initializeBank = do
    usersVar <- newTVar Map.empty
    amount <- newTVar 0
    bankAccountVar <- newTVar (Map.singleton bankCurrency amount)
    comission <- newTVar 0.01
    return $ Bank usersVar bankAccountVar comission

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

addAmount :: Bank -> String -> Currency -> Double -> STM ()
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
                comission <- readTVar (comission bank)
                let comissionAmount = calculateComissionAmount value comission
                newAccount <- newTVar (amount + value - comissionAmount)
                let changedAccount = Map.insert currency newAccount userAccounts 
                newAcc <- newTVar changedAccount 
                modifyTVar' (users bank) $ Map.insert username newAcc
                addMoneyToBank bank comissionAmount
        Nothing -> do
          error "There is no such user"

calculateComissionAmount :: Double -> Double -> Double
calculateComissionAmount value comission
    | value * comission < 1 = 1
    | otherwise = value * comission

withdrawMoney :: Bank -> String -> Currency -> Double -> STM ()
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

transferBetweenAccounts :: Bank -> String -> Currency -> Currency -> Double -> STM ()
transferBetweenAccounts bank username fromCurrency toCurrency amount = do
    withdrawMoney bank username fromCurrency amount
    addAmount bank username toCurrency amount

transferBetweenUsers :: Bank -> String -> String -> Currency -> Currency -> Double -> STM ()
transferBetweenUsers bank fromUser toUser fromCurrency toCurrency amount = do
    withdrawMoney bank fromUser fromCurrency amount
    addAmount bank toUser toCurrency amount

addMoneyToBank :: Bank -> Double -> STM ()
addMoneyToBank bank value = do
    bankAcc <- readTVar (bankAccount bank)
    case Map.lookup bankCurrency bankAcc of
              Nothing -> do
                error "There is no bank account"
              Just amountTVar -> do
                amount <- readTVar amountTVar
                comission <- readTVar (comission bank)
                newAccount <- newTVar (amount + value)
                modifyTVar' (bankAccount bank) $ Map.insert bankCurrency newAccount

someFunc :: IO ()
someFunc = do
    bank <- atomically initializeBank
    atomically $ do
        registerUser bank "User1"
        openAccount bank "User1" "RUB"
        addAmount bank "User1" "RUB" 200
    printBank bank
