module Parser where

import Control.Monad (void)
import Control.Monad.State (MonadState (get, put), MonadTrans (lift), State, evalState, gets)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Control.Monad.Trans.Writer (WriterT (runWriterT), tell)
import Data.Bifunctor (Bifunctor (first))
import Data.Maybe (listToMaybe)
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

peek :: Parser T.Token
peek = gets (head . snd)

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
            MaybeT $ return Nothing

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
parseExpression current precedence = prefixParse current

prefixParse :: T.Token -> MaybeT Parser N.Expression
prefixParse current
    | T.ttype current == T.Ident =
        return $ N.IdentifierExpr (N.Identifier current (T.literal current))
    | T.ttype current == T.Int = do
        case readMaybe $ T.literal current of
            Nothing -> do
                lift $ tell [printf "could not parse %s as integer" (T.literal current)]
                MaybeT $ return Nothing
            Just value -> return $ N.IntegerLiteralExpr current value
    | otherwise = MaybeT $ return Nothing

infixParse :: T.TokenType -> N.Expression -> MaybeT Parser N.Expression
infixParse tt lhs = MaybeT $ return Nothing

untilEOF :: Parser [N.Statement]
untilEOF = do
    current <- nextToken
    case current of
        T.Token T.EOF _ -> return []
        _ -> do
            stmt <- nextStatement
            maybe untilEOF (\s -> (s :) <$> untilEOF) stmt

runParser :: [T.Token] -> (Program, [String])
runParser [] = (Program [], [])
runParser ts = first Program . (`evalState` (undefined, ts)) . runWriterT $ untilEOF
