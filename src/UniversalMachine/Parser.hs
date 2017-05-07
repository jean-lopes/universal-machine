module UniversalMachine.Parser (
    parse
) where
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Token as Token
import qualified Text.Parsec.Language as Language
import Text.Parsec.String (Parser)
import Text.Parsec.Pos (SourceName)
import Text.Parsec.Error (messageString)
import qualified Data.Set as Set
import Data.List
import UniversalMachine

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser Language.emptyDef { Token.commentStart = "/*"
                                                , Token.commentEnd   = "*/"
                                                , Token.commentLine  = "//" }

charLiteral :: Parser Char
charLiteral = Token.charLiteral lexer

stringLiteral :: Parser String
stringLiteral = Token.stringLiteral lexer

identifier :: Parser String
identifier = Token.identifier lexer

lexeme :: Parser a -> Parser a
lexeme = Token.lexeme lexer

char :: Char -> Parser Char
char = lexeme . Parsec.char

parens :: Parser a -> Parser a
parens = Token.parens lexer

braces :: Parser a -> Parser a
braces = Token.braces lexer

commaSep1 :: Parser a -> Parser [a]
commaSep1 = Token.commaSep1 lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

symbolParser :: Parser Symbol
symbolParser = charLiteral

machineStateParser :: Parser State
machineStateParser = identifier

shiftParser :: Parser Shift
shiftParser = lexeme $ Parsec.oneOf "RL" >>= \c -> if c == 'R'
                                                      then return ShiftRight
                                                      else return ShiftLeft

transitionParser :: Parser Transition
transitionParser = parens $ do
    currentState <- machineStateParser
    char ','
    readSymbol <- symbolParser
    char ','
    writeSymbol <- symbolParser
    char ','
    shift <- shiftParser
    char ','
    newState <- machineStateParser
    return $ Transition currentState readSymbol writeSymbol shift newState

machineStatesParser :: Parser SetOfStates
machineStatesParser = braces $ Set.fromList <$> commaSep1 machineStateParser

transitionsParser :: Parser [Transition]
transitionsParser = braces $ commaSep1 transitionParser

machineParser :: Parser Machine
machineParser = do
    whiteSpace
    char 'M'
    char '='
    machineBodyParser

machineBodyParser :: Parser Machine
machineBodyParser = parens $ do
    ss <- machineStatesParser
    char ','
    as <- Set.fromList <$> stringLiteral
    char ','
    b <- symbolParser
    char ','
    ts <- transitionsParser
    char ','
    q0 <- machineStateParser
    char ','
    fs <- machineStatesParser
    return $ M { states = ss
               , alphabet = as
               , blank = b
               , transitions = ts
               , initialState = q0
               , finalStates = fs }

parse :: SourceName -> String -> Either String Machine
parse source xs = case (Parsec.parse machineParser source xs) of
                    (Left  er) -> Left $ show er
                    (Right mc) -> Right mc