{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
import Data.Monoid
import Data.Foldable
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State
--import Control.Monad.Trans.List
import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import Data.Semigroup
main = do
        let iParse = (runReaderT (string' "you")) ["cy","dict"]

                in print (runStateT iParse "you you")

kar :: Show a => [a] -> MaybeT IO a
kar xs = case xs of 
            [] -> MaybeT $ return Nothing                                  
            (x:xs) -> MaybeT $ do{putStrLn (show x); return $ Just x}

kdr :: Show a => [a] -> MaybeT IO [a] 
kdr xs = case xs of
            [] -> MaybeT $ return Nothing
            (x:xs) -> MaybeT $ do{putStrLn (show xs); return $ Just xs}
kadddr:: Show a => [a] -> MaybeT IO a
kadddr = kar <=< kdr <=< kdr <=< kdr

--2
type Parser a = StateT String Maybe a

item :: Parser Char
item = do { x <- get; put $ tail x; guard(x /= ""); return $ head x}

sat :: (Char -> Bool) -> Parser Char                                            
sat pred = do x <- item
              guard(pred x)
              return x

string :: String -> Parser String 
string (c:[]) = do sat (c ==)
                   return [c]
string (c:cs) = do sat (c ==)
                   string cs
                   return (c:cs)


--3 ask WILL GET GLOBAL STATE...but how do we get the result? runReaderT?
---strategy for item...
-- We know that our Parser' is a ReaderT whose result is a Parser
--So our strategy is to get the parser out and run (get) on the Parser we've extracted
--we can the proceed as we did with the first Parser combinators we wrote for this exercise

--runReaderT should pull a function out of the reader that we can use to get our Parser...we think.
--in the previous item, we were able to kick things off using get. That option isn't available to us here
--because we need to pull out the parser first.

type Dictionary = [String]
type Parser' a = ReaderT Dictionary (StateT String Maybe) a
--a
item' :: Parser' Char
item' = do
           c <- lift item
           ReaderT (\r -> (return c)) 

--b
sat' :: (Char -> Bool) -> Parser' Char
sat' pred = do x <- item'
               guard(pred x)
               return x
string' :: String -> Parser' String
string' (c:[]) = do sat' (c ==)
                    return [c]
string' (c:cs) = do sat' (c ==)
                    string' cs
                    return (c:cs)

--Stuck on word'
--First we need to verify that we correctly understand the behavior we want.

--Our impression is that, given an input, the parser should consume the first
--string if that first string is in its dictionary and fail otherwise. 
--Is this correct? 

--Our solution gets the dictionary and maps string' over it.
--We expect this to produce a list of (Parser' String) elements which we then fold to create
--a parser that will be successful on any of the elements in the dictionary.
--We believe our issue in implementing this solution is not knowing how to instance Parser' String as a monoid.

--instance monoid Parser' String was not accepted by the compiler
--We could use +++ or <|> to implement mappend 
--We believe mempty could just be return 

--Solution 2
--For solution 2, it's a recursive strategy. We have our list of valid strings from the dictionary
--that we get from the shared resource. By recursively going into the dictionary, we produce a 
--Parser' String at each step using the head element of the remaining dictionary.
--Upon failure we go deeper into recursion. Upon success, we append the result to a recursive call
--on the remaining string. 

--create 
-- word' :: Parser' String 
-- word' = do {dict <- ask; foldMap string' dict}
                 
instance Monoid (Parser' String) where
  mempty = mzero
  mappend = mplus

instance Semigroup (Parser' String) where
     (<>) = mappend
--4
data Btree a = Leaf a | Fork (Btree a)  (Btree a) deriving Show

btree :: (Read a, Show a) => [a] -> String -> Btree a
btree = undefined 