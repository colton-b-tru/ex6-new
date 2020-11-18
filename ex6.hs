{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
import Data.Monoid
import Data.Foldable
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import Data.Semigroup
import Control.Monad
import Control.Applicative
import Data.Char

main = do
        let iParse = (runReaderT (string' "miss")) ["cy","dict"]
            parserWord = (runReaderT word' ["ringo","george"])
            parseBeatle = (bTree beatles "Leaf John" )
                in do 
                        print (runStateT iParse "this that what")
                        print (runStateT parserWord "ringo yeah")
                        print parseBeatle
                        -- print (parseBeatle)


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


--create 
word' :: Parser' String 
word' = do {dict <- ask; foldMap string' dict}
                 
instance Monoid (Parser' String) where
  mempty = mzero
  mappend = mplus

instance Semigroup (Parser' String) where
     (<>) = mappend


--4
data Btree a = Leaf a | Fork (Btree a)  (Btree a) deriving Show

data Beatle = John | Paul | George | Ringo deriving (Show, Eq, Enum, Bounded, Read)
beatles = [(John)..(Ringo)]

bTree :: (Read a, Show a) => [a] -> String -> Btree a

bTree dict str = fst $ pullMaybe (runStateT (runReaderT bTreeParser (map show dict)) str )  

pullMaybe (Just a) = a 

bTreeParser :: (Read a, Show a) => Parser' (Btree a)
bTreeParser = do 
  pFork `mplus` pLeaf


pFork :: (Read a, Show a) => Parser' (Btree a)
pFork = do 
  string' "Fork"
  sat' (== ' ')
  btree0 <- bTreeParser
  sat' (== ' ')
  btree1 <- bTreeParser
  return $ Fork btree0 btree1
  `mplus` 
  do 
    sat' (== '(')
    fork0 <- pFork
    sat' (== ')')
    return fork0
pLeaf :: (Read a, Show a) => Parser' (Btree a)
pLeaf = do
  string' "Leaf"
  sat' (== ' ')
  leafWord <- word'
  return $ Leaf (read leafWord)
  `mplus` 
  do
    sat' (== '(')
    leaf0 <- pLeaf
    sat' (== ')')
    return leaf0
