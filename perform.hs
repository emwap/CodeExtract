{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternGuards #-}

module Main where

import Text.Pandoc
import Text.Pandoc.Generic
import Data.IORef
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import Control.Monad ((<=<))
import System.Process (readProcess)
import System.FilePath (splitExtension)

import Debug.Trace

type CodeMap = M.Map String [(String,String)]

writeTemplate :: String -> [(String, String)] -> IO ()
writeTemplate templateF vars
  = do template <- readFile templateF
       writeFile dest (renderTemplate vars template)
  where
    (dest,_) = splitExtension templateF

extractCode :: IORef CodeMap ->
               Block -> IO Block
extractCode m b@(CodeBlock (id,cls,attrs) code)
  | Just template <- lookup "template" attrs,
    Just var      <- lookup "var" attrs =
      do t <- readIORef m
         writeIORef m (M.insertWith (++) template [(var,code ++ "\n")] t)
         return b
extractCode m b = return b

processGHCI :: IORef CodeMap -> Block -> IO Block
processGHCI m b@(CodeBlock (label, cls, attrs) code)
    | "ghci" `elem` cls
    , Just template <- lookup "template" attrs
    = trace ("ghci block:" ++ code) $ do
      t <- readIORef m
      case M.lookup template t of
        Nothing -> trace "fail" $ return b
        Just source -> do
          writeTemplate template source
          raw <- readProcess "ghc" [fst $ splitExtension template, "-e", code] ""
          return $ CodeBlock (label, cls, attrs) $ "ghci> " ++ code ++ "\n" ++ raw
    | "perform" `elem` cls
    , Just template <- lookup "template" attrs
    = trace ("perform block: " ++ show template) $ do
      t <- readIORef m
      case M.lookup template t of
        Nothing -> return b
        Just source -> do
          writeTemplate template $ source ++ maybe [] (\n -> [("name",n)]) (lookup "name" attrs)
          raw <- readProcess "runhaskell" [fst $ splitExtension template] ""
          return $ RawBlock "latex" raw
processGHCI _ b = return b

main :: IO ()
main = do
    m <- newIORef M.empty
    toJsonFilter (processGHCI m <=< extractCode m)

