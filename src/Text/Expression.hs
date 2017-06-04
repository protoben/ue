module Text.Expression (expr, readExpr, funcCall) where

import Data.Expression
import Data.Units

import Text.Units
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr

import Control.Monad

type CParserT = ParsecT String ()

-- utility functions
inSpace :: Monad m => CParserT m a -> CParserT m a
inSpace p = spaces >> p >>= (\r->spaces >> return r)

symbol :: Monad m => String -> CParserT m ()
symbol s = (inSpace $ string s) >> return ()

name :: Monad m => CParserT m String
name = inSpace $ liftM2 (:) (letter <|> char '_') (many $ alphaNum <|> char '_')

-- constant value parsing
scalarValue :: Monad m => CParserT m Value
scalarValue = inSpace $ numericPart >>= (inSpace . addUnit) where
    numericPart = (try scientific) <|> (try decimal) <|> integer
    scientific = do
        (ExactReal decDigits decExp _) <- try decimal <|>
            liftM (\(IntValue x u)->ExactReal x 0 u) integer
        oneOf "eE"
        (IntValue exponent _) <- integer
        return $ ExactReal decDigits (decExp + exponent) noUnit
    decimal = do
        ipart <- many digit
        char '.'
        fpart <- many1 digit
        return $ ExactReal
            (read $ ipart ++ fpart)
            (-(fromIntegral $ length fpart))
            noUnit
    integer = do
        sign <- (string "+" >> return "") <|> string "-" <|> return ""
        digs <- many1 digit
        return $ IntValue (read (sign ++ digs)) noUnit

    -- parse and add a unit to a dimensionless quantity
    addUnit :: Monad m => Value -> CParserT m Value
    addUnit v = (liftM (\u->forceUnit u v) $ try unit) <|> (return v)

value :: Monad m => CParserT m Value
value = inSpace (vector <|> try scalarValue) where
    vector = liftM vectorPost $ between
        (symbol "<") (symbol ">") (sepBy scalarValue (symbol ","))

    vectorPost (x:y:[]) = Vec2 x y
    vectorPost xs = VecN xs

-- expression parsing
funcCall :: Monad m => CParserT m Expr
funcCall = try $ liftM2 FuncCall name (between
    (symbol "(") (symbol ")")
    (sepBy expr (symbol ",")))

term :: Monad m => CParserT m Expr
term = parenthesized <|> funcCall <|> nameRef <|> (liftM Constant value)
    where
        parenthesized = between (symbol "(") (symbol ")") mathExpr
        nameRef = liftM NameRef name

mathExpr :: Monad m => CParserT m Expr
mathExpr = buildExpressionParser exprTable term
    where
        exprTable = [
            [prefix "-" Negate],
            [binary "^" Power AssocRight],
            [binary "*" Multiply AssocLeft, binary "/" Divide AssocLeft],
            [binary "+" Add AssocLeft, binary "-" Subtract AssocLeft]]
        prefix c o = Prefix $ symbol c >> (return $ UnaryExpr o)
        binary c o a = Infix (symbol c >> (return $ BinaryExpr o)) a

expr :: Monad m => CParserT m Expr
expr = (try relation) <|> mathExpr
    where
        relation = do
            l <- mathExpr
            op <- choice [
                    try (symbol "=" >> return Equal),
                    try (symbol "<=" >> return Lesser),
                    try (symbol ">=" >> return Greater),
                    try (symbol "<" >> return Lesser),
                    try (symbol ">" >> return Greater)]
            r <- mathExpr

            return $ RelationExpr op l r

readExpr :: IO (Either ParseError Expr)
readExpr = liftM (parse expr "cmd") getLine
