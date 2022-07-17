{-# LANGUAGE FlexibleInstances #-}
module Lib
    ( someFunc
    ) where

data Token = LPAREN | RPAREN | DOT | TICK | STR String deriving (Eq)
instance Show Token where
    show LPAREN = "("
    show RPAREN = ")"
    show DOT = "."
    show TICK = "'"
    show (STR x) = x

getToken :: String -> (Maybe Token, String)
getToken "" = (Nothing, "")
getToken ('(':xs) = (Just LPAREN, xs)
getToken (')':xs) = (Just RPAREN, xs)
getToken ('.':xs) = (Just DOT, xs)
getToken ('\'':xs) = (Just TICK, xs)
getToken (' ':xs) = getToken xs
getToken ('\n':xs) = getToken xs
getToken ('\t':xs) = getToken xs
getToken (x:[]) = (Just (STR [x]), "")
getToken (x:y:xs) = if elem y ['(',')','.','\'',' ','\t','\n']
    then
        (Just (STR [x]), y:xs)
    else
        let (Just (STR z), zs) = getToken (y:xs) in
        (Just (STR (x:z)), zs)

type Tokens = [Token]
instance {-# OVERLAPS #-} Show Tokens where
    show ts = foldl (\l r -> l ++ " " ++ r ) "" $ map show ts

tokenize :: String -> Tokens
tokenize text =
    case maybeToken of  Just token -> token:(tokenize remainder)
                        Nothing -> []
    where (maybeToken, remainder) = getToken text

type Symbol = String
type LNumber = Int
type LList = [Atom]
data Atom = Symbol Symbol | LNumber LNumber | LList LList deriving (Eq, Show)

parse :: Tokens -> (Atom, Tokens)
parse ((STR first):ts) = (Symbol first,ts) -- TODO cast ints
parse (LPAREN:tokens) =
    (LList lst, ts)
    where (lst, ts) = parseList tokens
parse bad = error $ "Invalid expression " ++ show bad

-- parse a list, starting with its first element and stopping at )
parseList :: Tokens -> (LList, Tokens)
parseList (RPAREN:ts) = ([], ts)
parseList ts =
    (el:remainder, ts2)
    where (el,ts1) = parse ts
          (remainder,ts2) = parseList ts1

someFunc :: IO ()
-- someFunc = putStrLn "someFunc"
someFunc = do
    let t = tokenize "(add 4 (sub -7 8))"
    print t
    print $ parse [LPAREN,STR "foo",RPAREN]
    print $ parse [RPAREN]
    return ()