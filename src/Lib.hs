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
type Cons = (Atom, Atom)
data Atom = Symbol Symbol | LNumber LNumber | Cons Cons | Nil deriving (Eq)
instance Show Atom where
    show (Symbol s) = s
    show (LNumber n) = show n
    -- show (LList l) = "(" ++ (foldl (\l r -> l ++ " " ++ r) "" $ map show l) ++ " )"
    show (Cons (a, b)) = (show a) ++ " . " ++ (show b)
    show Nil = "nil"

parse :: Tokens -> (Atom, Tokens)
parse ((STR first):ts) = (Symbol first,ts) -- TODO cast ints
parse (LPAREN:tokens) = parseList tokens
parse bad = error $ "Invalid expression " ++ show bad

-- parse a list, starting with its first element and stopping at )
parseList :: Tokens -> (Atom, Tokens) -- ??? maybe this should be typed cons and cons should also include nil
parseList (RPAREN:ts) = (Nil, ts)
parseList ts =
    (Cons (el,remainder), ts2)
    where (el,ts1) = parse ts
          (remainder,ts2) = parseList ts1

eval :: Atom -> Atom
eval (Cons (Symbol s, args))
    | s == "foo" = LNumber 5
    | otherwise = error $ "Undefined function " ++ s
eval (Cons (a, args)) = error $ "First argument must be function name, instead got " ++ (show a)
eval x = x

someFunc :: IO ()
someFunc = do
    let t = tokenize "(add 4 (sub -7 8))"
    print t
    let parsed = fst $ parse $ tokenize "(foo    bar)"
    print parsed
    print $ eval parsed
    return ()
