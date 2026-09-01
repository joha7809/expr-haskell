{-# LANGUAGE LambdaCase #-}

import Data.Char (isDigit, isSpace)
import Data.Functor
import Text.Read (readMaybe)

main :: IO ()
main = undefined

data Expr
    = Lit Int
    | Add Expr Expr
    | Mul Expr Expr
    | Neg Expr
    deriving (Show)

precedence :: Expr -> Int
precedence (Lit _) = 3
precedence (Neg _) = 2
precedence (Mul _ _) = 1
precedence (Add _ _) = 0

eval :: Expr -> Int
eval (Lit n) = n
eval (Add left right) = eval left + eval right
eval (Mul left right) = eval left * eval right
eval (Neg expr) = -(eval expr)

pretty :: Expr -> String
pretty = prettyWithPrecedence 0

prettyWithPrecedence :: Int -> Expr -> String
prettyWithPrecedence parentPrec expr
    | precedence expr < parentPrec = "(" ++ prettyExpr expr ++ ")"
    | otherwise = prettyExpr expr

prettyExpr :: Expr -> String
prettyExpr (Lit n) = show n
prettyExpr expr@(Add left right) = prettyWithPrecedence (precedence expr) left ++ " + " ++ prettyWithPrecedence (precedence expr) right
prettyExpr expr@(Neg inner) = "-" ++ prettyWithPrecedence (precedence expr) inner
prettyExpr expr@(Mul left right) = prettyWithPrecedence (precedence expr) left ++ " * " ++ prettyWithPrecedence (precedence expr) right

-- Parser a parses a sting returning captured type a alongside the remainder of the input
newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

instance Functor Parser where
    fmap f (Parser p) = Parser $ \input ->
        case p input of
            Nothing -> Nothing
            Just (a, rest) -> Just (f a, rest)

instance Applicative Parser where
    pure a = Parser $ \input -> Just (a, input)
    a <*> b = Parser $ \input -> case runParser a input of
        Nothing -> Nothing
        Just (f, rest) -> runParser (fmap f b) rest

instance Monad Parser where
    a >>= b = bind a b

-- If I have a Parser a, and a function that takes the parsed a and produces another parser, what should the resulting parser do?
bind :: Parser a -> (a -> Parser b) -> Parser b
bind x f = Parser $ \input ->
    case runParser x input of
        Just (a, rest) -> runParser (f a) rest
        Nothing -> Nothing

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \input ->
    case input of
        [] -> Nothing
        (c : rest) -> if f c then Just (c, rest) else Nothing

parseChar :: Char -> Parser Char
parseChar c = satisfy $ \c' -> c == c'

parseDigit = satisfy $ \c -> isDigit c

orElse :: Parser a -> Parser a -> Parser a
orElse f g = Parser $ \input ->
    case runParser f input of
        Just (a, rest) -> Just (a, rest)
        Nothing -> runParser g input

--- absolute cinema
many :: Parser a -> Parser [a]
many p =
    ( do
        x <- p
        xs <- many p
        pure (x : xs)
    )
        `orElse` pure []

failParser :: Parser a
failParser = Parser $ \_ -> Nothing

parseInt :: Parser Int
parseInt = do
    result <- many parseDigit
    case readMaybe result of
        Just n -> pure n
        Nothing -> failParser

ws :: Parser [Char]
ws = many (satisfy $ \c -> isSpace c)

exprParser :: Parser Expr
exprParser = addParser

addParser :: Parser Expr
addParser = do
    first <- mulParser
    rest <- many $ do
        ws
        parseChar '+'
        ws
        mulParser
    pure $ foldl Add first rest

mulParser :: Parser Expr
mulParser = do
    first <- negParser
    rest <- many $ do
        ws
        parseChar '*'
        ws
        negParser
    pure $ foldl Mul first rest

negParser :: Parser Expr
negParser =
    ( do
        ws
        parseChar '-'
        ws
        Neg <$> negParser
    )
        `orElse` atomParser

atomParser :: Parser Expr
atomParser =
    (Lit <$> parseInt)
        `orElse` ( do
                    parseChar '('
                    expr <- exprParser
                    parseChar ')'
                    pure expr
                 )
