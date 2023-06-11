module Parser where

import Control.Monad (void)
import Control.Monad.State (MonadState (get, put), MonadTrans (lift), State, evalState, gets)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT), hoistMaybe)
import Control.Monad.Trans.Writer (WriterT (runWriterT), tell)
import Data.Bifunctor (Bifunctor (first))
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Nodes as N
import Text.Printf (printf)
import Text.Read (readMaybe)
import qualified Tokens as T

type Parser = WriterT [String] (State (T.Token, [T.Token]))

newtype Program = Program [N.Statement]

instance Show Program where
    show (Program stmts) = concatMap show stmts

data Precedence
    = Lowest
    | Equals
    | LessGreater
    | Sum
    | Product
    | Prefix
    | Call
    deriving (Eq, Ord)

precedence :: T.TokenType -> Precedence
precedence T.Equal = Equals
precedence T.NotEqual = Equals
precedence T.LessThan = LessGreater
precedence T.GreaterThan = LessGreater
precedence T.Plus = Sum
precedence T.Minus = Sum
precedence T.Slash = Product
precedence T.Asterisk = Product
precedence _ = Lowest

peek :: Parser T.Token
peek = gets (head . snd)

tokenPrecedence :: T.Token -> Precedence
tokenPrecedence = precedence . T.ttype

peekPrecedence :: Parser Precedence
peekPrecedence = tokenPrecedence <$> peek

currentToken :: Parser T.Token
currentToken = gets fst

nextStatement :: Parser (Maybe N.Statement)
nextStatement = do
    current <- currentToken
    runMaybeT $ case T.ttype current of
        T.Let -> parseLet current
        T.Return -> parseReturn current
        _ -> parseExpressionStmt current

nextToken :: Parser T.Token
nextToken = do
    ~(_, next : rest) <- get
    put (next, rest)
    return next

expectToken :: T.TokenType -> MaybeT Parser T.Token
expectToken tt = do
    next <- lift peek
    if T.ttype next == tt
        then lift nextToken
        else do
            lift $ tell [printf "expected next token to be %s, got %s instead" (show tt) (show . T.ttype $ next)]
            hoistMaybe Nothing

eatUntil :: T.TokenType -> Parser ()
eatUntil tt = do
    token <- nextToken
    if T.ttype token == tt
        then return ()
        else eatUntil tt

parseLet :: T.Token -> MaybeT Parser N.Statement
parseLet current = do
    identToken <- expectToken T.Ident
    assignToken <- expectToken T.Assign
    lift $ eatUntil T.Semicolon

    let identifier = N.Identifier identToken (T.literal identToken)
    return (N.LetStmt current identifier N.MissingExpr)

parseReturn :: T.Token -> MaybeT Parser N.Statement
parseReturn current = do
    lift $ eatUntil T.Semicolon
    return (N.ReturnStmt current N.MissingExpr)

parseExpressionStmt :: T.Token -> MaybeT Parser N.Statement
parseExpressionStmt current = do
    expr <- parseExpression current Lowest
    maybeNext <- gets (listToMaybe . snd)
    case maybeNext of
        Just (T.Token T.Semicolon _) -> void $ lift nextToken
        _ -> return ()
    return (N.ExpressionStmt current expr)

parseExpression :: T.Token -> Precedence -> MaybeT Parser N.Expression
parseExpression current precedence = do
    lhs <- parsePrefix current
    parseInfix lhs precedence

parsePrefix :: T.Token -> MaybeT Parser N.Expression
parsePrefix current = case T.ttype current of
    T.Ident -> return $ N.IdentifierExpr (N.Identifier current (T.literal current))
    T.Int -> case readMaybe $ T.literal current of
        Nothing -> do
            lift $ tell [printf "could not parse %s as integer" (T.literal current)]
            hoistMaybe Nothing
        Just value -> return $ N.IntegerLiteralExpr current value
    T.True -> return $ N.BooleanExpr current True
    T.False -> return $ N.BooleanExpr current False
    T.Bang -> parsePrefixExpression current
    T.Minus -> parsePrefixExpression current
    T.LParen -> parseGroupedExpression
    T.If -> parseIfExpression current
    _ -> do
        lift . tell $ [printf "no prefix parse function for %s found" $ show (T.ttype current)]
        hoistMaybe Nothing

parsePrefixExpression :: T.Token -> MaybeT Parser N.Expression
parsePrefixExpression current = do
    next <- lift nextToken
    rhs <- parseExpression next Prefix
    return $ N.PrefixExpression current (T.literal current) rhs

parseGroupedExpression :: MaybeT Parser N.Expression
parseGroupedExpression = do
    current <- lift nextToken
    expr <- parseExpression current Lowest
    expectToken T.RParen
    return expr

parseIfExpression :: T.Token -> MaybeT Parser N.Expression
parseIfExpression current = do
    expectToken T.LParen
    next <- lift nextToken
    condition <- parseExpression next Lowest

    expectToken T.RParen
    expectToken T.LBrace
    consequence <- parseBlock

    peekToken <- lift $ gets (listToMaybe . snd)
    case T.ttype <$> peekToken of
        Just T.Else -> do
            lift nextToken
            expectToken T.LBrace
            N.IfExpression current condition consequence . Just <$> parseBlock
        _ -> return $ N.IfExpression current condition consequence Nothing

parseBlock :: MaybeT Parser N.Block
parseBlock = N.Block <$> lift (untilToken [T.RBrace, T.EOF])

parseInfix :: N.Expression -> Precedence -> MaybeT Parser N.Expression
parseInfix lhs precedence = do
    peekedToken <- lift peek
    peekedPrecedence <- lift peekPrecedence
    if T.ttype peekedToken == T.Semicolon || precedence >= peekedPrecedence
        then return lhs
        else do
            current <- lift nextToken
            lhs <- parseInfixExpression current lhs
            parseInfix lhs precedence

parseInfixExpression :: T.Token -> N.Expression -> MaybeT Parser N.Expression
parseInfixExpression current lhs =
    if T.ttype current `notElem` [T.Plus, T.Minus, T.Slash, T.Asterisk, T.Equal, T.NotEqual, T.LessThan, T.GreaterThan]
        then return lhs
        else do
            let precedence = tokenPrecedence current
            next <- lift nextToken
            rhs <- parseExpression next precedence
            return $ N.InfixExpression current lhs (T.literal current) rhs

untilToken :: [T.TokenType] -> Parser [N.Statement]
untilToken ttypes = do
    current <- nextToken
    if T.ttype current `elem` ttypes
        then return []
        else do
            stmt <- nextStatement
            (fromMaybe N.InvalidStatement stmt :) <$> untilToken ttypes

runParser :: [T.Token] -> (Program, [String])
runParser [] = (Program [], [])
runParser ts = first Program . (`evalState` (undefined, ts)) . runWriterT $ untilToken [T.EOF]
