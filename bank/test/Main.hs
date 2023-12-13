{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Tasty
import Test.Tasty.Hspec
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog
import Hedgehog
import Control.Concurrent.STM
import qualified Data.Map as Map
import Control.Monad
import Control.Exception

import MyLib

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "MyLib Tests"
  [ initializeBankTests
  , userRegistrationTests
  , accountOperationTests
  , transferTests
  , userDeletingTests
  , calculatingComissionTests
  ]

initializeBankTests :: TestTree
initializeBankTests = testGroup "Initialize Bank"
  [ testCase "Bank is initialized with empty user list and bank account" $ do
        bank <- atomically initializeBank
        usersMap <- atomically $ readTVar (users bank)
        bankAccountMap <- atomically $ readTVar (bankAccount bank)
        comissionRate <- atomically $ readTVar (comission bank)
        Map.size usersMap @?= 0
        Map.size bankAccountMap @?= 1
        let bankAccount = Map.toList bankAccountMap
        forM_ (bankAccount) $ \(k,v) -> do
            k @?= "USD"
            bankAccountAmount <- atomically $ readTVar v
            bankAccountAmount @?= 0
        comissionRate @?= 0.01
  ]

userRegistrationTests :: TestTree
userRegistrationTests = testGroup "User Registration"
  [ testCase "Register a new user" $ do
        bank <- atomically initializeBank
        atomically $ registerUser bank "User1"
        usersMap <- atomically $ readTVar (users bank)
        Map.size usersMap @?= 1
        let accountTVar = Map.lookup "User1" usersMap
        account <- atomically $ readTVar $ eliminate accountTVar
        Map.size account @?= 0
  , testCase "Registering an existing user does not change anything" $ do
        bank <- atomically initializeBank
        atomically $ do
            registerUser bank "User1"
            registerUser bank "User1"
        usersMap <- atomically $ readTVar (users bank)
        Map.size usersMap @?= 1
  ]

accountOperationTests :: TestTree
accountOperationTests = testGroup "Account Operations"
  [ testCase "Open account for a user" $ do
        bank <- atomically initializeBank
        atomically $ do
            registerUser bank "User1"
            openAccount bank "User1" "EUR"
        usersMap <- atomically $ readTVar (users bank)
        let userAccounts = Map.lookup "User1" usersMap
        case userAccounts of
            Just accs -> do
                accsMap <- atomically $ readTVar accs 
                amount <- atomically $ readTVar $ eliminate $ Map.lookup "EUR" accsMap
                amount @?= 0
            Nothing -> assertFailure "User not found"
  ,  testCase "Open 2 accounts for a user" $ do
        bank <- atomically initializeBank
        atomically $ do
            registerUser bank "User1"
            openAccount bank "User1" "EUR"
            openAccount bank "User1" "USD"
        usersMap <- atomically $ readTVar (users bank)
        let userAccounts = Map.lookup "User1" usersMap
        case userAccounts of
            Just accs -> do
                accsMap <- atomically $ readTVar accs 
                amountEUR <- atomically $ readTVar $ eliminate $ Map.lookup "EUR" accsMap
                amountEUR @?= 0
                amountUSD <- atomically $ readTVar $ eliminate $ Map.lookup "USD" accsMap
                amountUSD @?= 0
            Nothing -> assertFailure "User not found"
  ,  testCase "Open existing account for a user" $ do
        bank <- atomically initializeBank
        atomically $ do
            registerUser bank "User1"
            openAccount bank "User1" "EUR"
            openAccount bank "User1" "EUR"
        usersMap <- atomically $ readTVar (users bank)
        let userAccounts = Map.lookup "User1" usersMap
        case userAccounts of
            Just accs -> do
                accsMap <- atomically $ readTVar accs 
                amountEUR <- atomically $ readTVar $ eliminate $ Map.lookup "EUR" accsMap
                amountEUR @?= 0
                Map.size accsMap @?= 1
            Nothing -> assertFailure "User not found"
  ,  testCase "Delete empty account" $ do
        bank <- atomically initializeBank
        atomically $ do
            registerUser bank "User1"
            openAccount bank "User1" "EUR"
            closeAccount bank "User1" "EUR"
        usersMap <- atomically $ readTVar (users bank)
        let accountTVar = Map.lookup "User1" usersMap
        account <- atomically $ readTVar $ eliminate accountTVar
        Map.size account @?= 0
  ,  testCase "should not delete user with non-empty account" $ do
        bank <- atomically initializeBank
        atomically $ do
            registerUser bank "User1"
            openAccount bank "User1" "USD"
            addAmount bank "User1" "USD" 100
        errored <- catch (atomically $ closeAccount bank "User1" "USD" >> pure False) handler
        if errored then
            pure ()
        else
            assertFailure "Did not catch expected error"
  ,  testCase "Delete empty account when user has 2 accounts" $ do
        bank <- atomically initializeBank
        atomically $ do
            registerUser bank "User1"
            openAccount bank "User1" "EUR"
            openAccount bank "User1" "USD"
            closeAccount bank "User1" "EUR"
        usersMap <- atomically $ readTVar (users bank)
        let accountTVar = Map.lookup "User1" usersMap
        let userAccounts = Map.lookup "User1" usersMap
        case userAccounts of
            Just accs -> do
                accsMap <- atomically $ readTVar accs 
                amount <- atomically $ readTVar $ eliminate $ Map.lookup "USD" accsMap
                amount @?= 0
                Map.size accsMap @?= 1
            Nothing -> assertFailure "User not found"
  ]

transferTests :: TestTree
transferTests = testGroup "Transfer Operations"
  [ testCase "Transfer between accounts" $ do
        bank <- atomically initializeBank
        atomically $ do
            registerUser bank "User1"
            openAccount bank "User1" "USD"
            openAccount bank "User1" "EUR"
            addAmount bank "User1" "USD" 500
            transferBetweenAccounts bank "User1" "USD" "EUR" 200
        usersMap <- atomically $ readTVar (users bank)
        let userAccounts = Map.lookup "User1" usersMap
        case userAccounts of
            Just accs -> do
                accsMap <- atomically $ readTVar accs
                amountUSD <- atomically $ readTVar (fromJust $ Map.lookup "USD" accsMap)
                amountEUR <- atomically $ readTVar (fromJust $ Map.lookup "EUR" accsMap)
                comission <- atomically $ readTVar (comission bank)
                amountUSD @?= 295
                amountEUR @?= 200 * (1 - comission)
            Nothing -> assertFailure "User not found"
  , testCase "Transfer between users" $ do
        bank <- atomically initializeBank
        atomically $ do
            registerUser bank "User1"
            registerUser bank "User2"
            openAccount bank "User1" "USD"
            openAccount bank "User2" "EUR"
            addAmount bank "User1" "USD" 500
            transferBetweenUsers bank "User1" "User2" "USD" "EUR" 200
        usersMap <- atomically $ readTVar (users bank)
        let user1Accounts = Map.lookup "User1" usersMap
        case user1Accounts of
            Just accs -> do
                accsMap <- atomically $ readTVar accs
                amountUSD <- atomically $ readTVar (fromJust $ Map.lookup "USD" accsMap)
                comission <- atomically $ readTVar (comission bank)
                amountUSD @?= 295
            Nothing -> assertFailure "User not found"
        let user2Accounts = Map.lookup "User2" usersMap
        case user2Accounts of
            Just accs -> do
                accsMap <- atomically $ readTVar accs
                amountUSD <- atomically $ readTVar (fromJust $ Map.lookup "EUR" accsMap)
                comission <- atomically $ readTVar (comission bank)
                amountUSD @?= 198
            Nothing -> assertFailure "User not found"
  , testCase "Adding value to account" $ do
        bank <- atomically initializeBank
        atomically $ do
            registerUser bank "User1"
            registerUser bank "User2"
            openAccount bank "User1" "USD"
            openAccount bank "User1" "EUR"
            addAmount bank "User1" "USD" 500
        usersMap <- atomically $ readTVar (users bank)
        let user1Accounts = Map.lookup "User1" usersMap
        case user1Accounts of
            Just accs -> do
                accsMap <- atomically $ readTVar accs
                amountUSD <- atomically $ readTVar (fromJust $ Map.lookup "USD" accsMap)
                comission <- atomically $ readTVar (comission bank)
                amountUSD @?= 495
                amountEUR <- atomically $ readTVar (fromJust $ Map.lookup "EUR" accsMap)
                amountEUR @?= 0
            Nothing -> assertFailure "User not found"
        let user2Accounts = Map.lookup "User2" usersMap
        case user2Accounts of
            Just accs -> do
                accsMap <- atomically $ readTVar accs
                Map.size accsMap @?= 0
            Nothing -> assertFailure "User not found"
  , testCase "Withdraw value from account" $ do
        bank <- atomically initializeBank
        atomically $ do
            registerUser bank "User1"
            registerUser bank "User2"
            openAccount bank "User1" "USD"
            openAccount bank "User1" "EUR"
            addAmount bank "User1" "USD" 500
            withdrawMoney bank "User1" "USD" 100
        usersMap <- atomically $ readTVar (users bank)
        let user1Accounts = Map.lookup "User1" usersMap
        case user1Accounts of
            Just accs -> do
                accsMap <- atomically $ readTVar accs
                amountUSD <- atomically $ readTVar (fromJust $ Map.lookup "USD" accsMap)
                comission <- atomically $ readTVar (comission bank)
                amountUSD @?= 395
                amountEUR <- atomically $ readTVar (fromJust $ Map.lookup "EUR" accsMap)
                amountEUR @?= 0
            Nothing -> assertFailure "User not found"
        let user2Accounts = Map.lookup "User2" usersMap
        case user2Accounts of
            Just accs -> do
                accsMap <- atomically $ readTVar accs
                Map.size accsMap @?= 0
            Nothing -> assertFailure "User not found"
  , testCase "Withdraw value from empty account" $ do
        bank <- atomically initializeBank
        atomically $ do
            registerUser bank "User1"
            registerUser bank "User2"
            openAccount bank "User1" "USD"
            openAccount bank "User1" "EUR"
        errored <- catch (atomically $ withdrawMoney bank "User1" "USD" 10 >> pure False) handler
        if errored then
            pure ()
        else
            assertFailure "Did not catch expected error"
  ]

userDeletingTests :: TestTree
userDeletingTests = testGroup "User Deleting"
  [ testCase "Delete a new user" $ do
        bank <- atomically initializeBank
        atomically $ registerUser bank "User1"
        atomically $ deleteUser bank "User1"
        usersMap <- atomically $ readTVar (users bank)
        Map.size usersMap @?= 0
  , testCase "Delete a new user when there are 2 users" $ do
        bank <- atomically initializeBank
        atomically $ do
            registerUser bank "User1"
            registerUser bank "User2"
            deleteUser bank "User1"
        usersMap <- atomically $ readTVar (users bank)
        Map.size usersMap @?= 1
        let accountTVar = Map.lookup "User2" usersMap
        assertBool "User found" (accountTVar /= Nothing)
  , testCase "Delete a user with account" $ do
        bank <- atomically initializeBank
        atomically $ do
            registerUser bank "User1"
            openAccount bank "User1" "USD"
        errored <- catch (atomically $ deleteUser bank "User1" >> pure False) handler
        if errored then
            pure ()
        else
            assertFailure "Did not catch expected error"
  ]

calculatingComissionTests :: TestTree
calculatingComissionTests = testGroup "Calculating Comission"
  [ testCase "Calculating comision when 1% < 1" $ do
        calculateComissionAmount 10 0.01 @?= 1
  , testCase "Calculating comision when 1% > 1" $ do
        calculateComissionAmount 1000 0.01 @?= 10
  ]

eliminate (Just a) = a
eliminate Nothing = undefined

fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing  = error "fromJust: Nothing"

handler :: ErrorCall -> IO Bool
handler _ = pure True
