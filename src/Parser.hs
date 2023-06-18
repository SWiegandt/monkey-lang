module Parser where

import Control.Applicative (Applicative (liftA2))
import Control.Monad (void, when)
import Control.Monad.State.Strict (MonadState (put), MonadTrans (lift), State, evalState, gets)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT), hoistMaybe)
import Control.Monad.Trans.Writer (WriterT (runWriterT), tell)
import Data.Bifunctor (Bifunctor (first))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, listToMaybe)
import qualified Nodes as N
import Text.Printf (printf)
import Text.Read (readMaybe)
import qualified Tokens as T

type Parser = WriterT [String] (State (T.Token, [T.Token]))

data Precedence
    = Lowest
    | Equals
    | LessGreater
    | Sum
    | Product
    | Prefix
    | Call
    | Index
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
precedence T.LParen = Call
precedence T.LBracket = Index
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
    case T.ttype current of
        T.Let -> runMaybeT parseLet
        T.Return -> runMaybeT parseReturn
        _ -> parseExpressionStmt

nextToken :: Parser T.Token
nextToken = do
    rest <- gets snd
    case rest of
        (next : rest') -> put (next, rest') >> return next
        _ -> return $ T.Token T.EOF ""

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
    when (T.ttype token == tt) $ eatUntil tt

skipIfSemicolon :: Parser ()
skipIfSemicolon = do
    peekToken <- gets (listToMaybe . snd)
    when (T.Semicolon `elem` (T.ttype <$> peekToken)) $ void nextToken

parseLet :: MaybeT Parser N.Statement
parseLet =
    N.LetStmt
        <$> lift currentToken
        <*> ((\ident -> N.Identifier ident (T.literal ident)) <$> expectToken T.Ident)
        <* expectToken T.Assign
        <* lift nextToken
        <*> parseExpression Lowest
        <* lift skipIfSemicolon

parseReturn :: MaybeT Parser N.Statement
parseReturn =
    N.ReturnStmt
        <$> lift currentToken
        <* lift nextToken
        <*> parseExpression Lowest
        <* lift skipIfSemicolon

parseExpressionStmt :: Parser (Maybe N.Statement)
parseExpressionStmt =
    (\token -> (N.ExpressionStmt token <$>))
        <$> currentToken
        <*> runMaybeT (parseExpression Lowest)
        <* skipIfSemicolon

parseExpression :: Precedence -> MaybeT Parser N.Expression
parseExpression precedence = do
    current <- lift currentToken
    lhs <- parsePrefix current
    parseInfix lhs precedence

parsePrefix :: T.Token -> MaybeT Parser N.Expression
parsePrefix current = case T.ttype current of
    T.Ident -> return $ N.IdentifierExpression (N.Identifier current (T.literal current))
    T.Int -> case readMaybe $ T.literal current of
        Nothing -> do
            lift $ tell [printf "could not parse %s as integer" (T.literal current)]
            hoistMaybe Nothing
        Just value -> return $ N.IntegerExpression current value
    T.True -> return $ N.BooleanExpression current True
    T.False -> return $ N.BooleanExpression current False
    T.String -> return $ N.StringExpression current (T.literal current)
    T.Bang -> parsePrefixExpression
    T.Minus -> parsePrefixExpression
    T.LParen -> parseGroupedExpression
    T.If -> parseIfExpression
    T.Function -> parseFunctionExpression
    T.LBracket -> parseArrayExpression
    T.LBrace -> parseHashExpression
    _ -> do
        lift . tell $ [printf "no prefix parse function for %s found" $ show (T.ttype current)]
        hoistMaybe Nothing

parsePrefixExpression :: MaybeT Parser N.Expression
parsePrefixExpression =
    (\token rhs -> N.PrefixExpression token (T.literal token) rhs)
        <$> lift currentToken
        <* lift nextToken
        <*> parseExpression Prefix

parseInfix :: N.Expression -> Precedence -> MaybeT Parser N.Expression
parseInfix lhs precedence = do
    peekedToken <- lift peek
    peekedPrecedence <- lift peekPrecedence
    if T.ttype peekedToken == T.Semicolon || precedence >= peekedPrecedence
        then return lhs
        else do
            current <- lift nextToken
            lhs <- parseLHS current lhs
            parseInfix lhs precedence

parseLHS :: T.Token -> N.Expression -> MaybeT Parser N.Expression
parseLHS current lhs
    | T.ttype current
        `elem` [ T.Plus,
                 T.Minus,
                 T.Slash,
                 T.Asterisk,
                 T.Equal,
                 T.NotEqual,
                 T.LessThan,
                 T.GreaterThan
               ] =
        N.InfixExpression current lhs (T.literal current)
            <$> (lift nextToken *> parseExpression (tokenPrecedence current))
    | T.ttype current == T.LParen = N.CallExpression current lhs <$> parseExpressionList T.RParen (parseExpression Lowest)
    | T.ttype current == T.LBracket = N.IndexExpression current lhs <$> parseIndexExpression
    | otherwise = return lhs

parseGroupedExpression :: MaybeT Parser N.Expression
parseGroupedExpression = lift nextToken *> parseExpression Lowest <* expectToken T.RParen

parseIfExpression :: MaybeT Parser N.Expression
parseIfExpression = do
    current <- lift currentToken
    expectToken T.LParen
    next <- lift nextToken
    condition <- parseExpression Lowest

    expectToken T.RParen
    expectToken T.LBrace
    consequence <- parseBlock

    peekToken <- lift $ gets (listToMaybe . snd)
    case T.ttype <$> peekToken of
        Just T.Else ->
            N.IfExpression current condition consequence . Just
                <$> (lift nextToken *> expectToken T.LBrace *> parseBlock)
        _ -> return $ N.IfExpression current condition consequence Nothing

parseFunctionExpression :: MaybeT Parser N.Expression
parseFunctionExpression =
    N.FunctionExpression
        <$> lift currentToken
        <* expectToken T.LParen
        <*> parseFunctionParams
        <* expectToken T.LBrace
        <*> parseBlock

parseExpressionList :: T.TokenType -> MaybeT Parser a -> MaybeT Parser [a]
parseExpressionList end parser = do
    peekToken <- lift peek
    if T.ttype peekToken == end
        then lift nextToken >> return []
        else appendParam (lift nextToken) <* expectToken end
    where
        rest = do
            peekToken <- lift peek
            case T.ttype peekToken of
                T.Comma -> appendParam (lift nextToken *> lift nextToken)
                _ -> return []
        appendParam position = liftA2 (:) (position *> parser) rest

parseFunctionParams :: MaybeT Parser [N.Identifier]
parseFunctionParams = parseExpressionList T.RParen $ do
    current <- lift currentToken
    return $ N.Identifier current (T.literal current)

parseArrayExpression :: MaybeT Parser N.Expression
parseArrayExpression =
    N.ArrayExpression
        <$> lift currentToken
        <*> parseExpressionList T.RBracket (parseExpression Lowest)

parseIndexExpression :: MaybeT Parser N.Expression
parseIndexExpression = lift nextToken *> parseExpression Lowest <* expectToken T.RBracket

parseHashExpression :: MaybeT Parser N.Expression
parseHashExpression =
    N.HashExpression
        <$> lift currentToken
        <*> (Map.fromList <$> parseExpressionList T.RBrace parseHashKeyValue)

parseHashKeyValue :: MaybeT Parser (N.Expression, N.Expression)
parseHashKeyValue =
    (,)
        <$> parseExpression Lowest
        <* expectToken T.Colon
        <* lift nextToken
        <*> parseExpression Lowest

parseBlock :: MaybeT Parser N.Block
parseBlock = N.Block <$> lift (untilToken [T.RBrace, T.EOF])

untilToken :: [T.TokenType] -> Parser [N.Statement]
untilToken ttypes = do
    current <- nextToken
    if T.ttype current `elem` ttypes
        then return []
        else (\stmt -> (fromJust stmt :)) <$> nextStatement <*> untilToken ttypes

runParser :: [T.Token] -> (N.Program, [String])
runParser [] = (N.Program [], [])
runParser ts = first N.Program . (`evalState` (undefined, ts)) . runWriterT $ untilToken [T.EOF]
