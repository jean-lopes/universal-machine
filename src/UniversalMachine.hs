module UniversalMachine {- (
    Symbol,
    Blank,
    Alphabet,
    State,
    SetOfStates,
    Tape,
    Shift(..),
    Transition(..),
    Machine(..),
    UniversalMachine(..),
    mkUniversalMachine,
    eval,
    view
) -} where
import Prelude hiding (index)
import Data.List as List
import Data.Set as Set hiding (toList)
import Data.Sequence as Seq
import Data.Foldable (toList)
import Control.Monad.State.Lazy hiding (State, state)
import Control.Monad.Writer
import Control.Monad.Identity

type Symbol = Char

type Blank = Symbol

type Location = String

type Alphabet = [Symbol]

type State = String

type SetOfStates = Set State

type Tape = Seq Symbol

data Shift = ShiftLeft
           | ShiftRight
           deriving (Eq, Ord)

instance Show Shift where
    show ShiftLeft  = "L"
    show ShiftRight = "R"

data Transition = Transition { currentState :: State
                             , inputSymbol :: Symbol
                             , outputSymbol :: Symbol
                             , shift :: Shift
                             , newState :: State
                             } deriving (Eq, Ord)

instance Show Transition where
    show (Transition c i o s n) = "(" ++ c ++ "," ++ show i ++ "," ++ show o ++ "," ++ show s ++ "," ++ n ++ ")"

data Machine = M { states :: SetOfStates
                 , alphabet :: Set Symbol
                 , blank :: Symbol
                 , transitions :: [Transition]
                 , initialState :: State
                 , finalStates :: SetOfStates
                 } deriving (Eq, Ord)

instance Show Machine where
    show m = "M = (" ++ ss ++ ", " ++ as ++ ", " ++ b ++ ", " ++ ts ++ ", " ++ i ++ ", " ++ fs ++ ")"
        where ss = setOfStatesToString $ states m
              as = listToString $ toList $ alphabet m
              b = show $ blank m
              ts = listToString $ transitions m
              i = initialState m
              fs = setOfStatesToString $ finalStates m

data UniversalMachine = UM { tape :: Tape
                           , machine :: Machine
                           , index :: Int
                           , state :: State
                           } deriving (Eq, Ord)

instance Show UniversalMachine where
    show um = "UM = (state = " ++ state um ++ ", index = " ++ is ++ ", tape = " ++ ts ++ ")"
        where is = show $ UniversalMachine.index um
              ms = show $ machine um
              ts = show $ tape um

data RunTimeError = TapeIndexOutOfBound Int
                  | NoSuchTransition State Symbol
                  | MultipleTransitionsFound State Symbol [Transition]
                  | AlphabetTooShort
                  | UndeclaredSymbol Location Alphabet Symbol
                  | UndeclaredState Location SetOfStates State

instance Show RunTimeError where
    show (TapeIndexOutOfBound n) = "Tape index out of bound. (index: " ++ show n ++ ")"
    show (NoSuchTransition st s) = "No transition found for state " ++ st ++ " with input symbol " ++ show s ++ "."
    show (MultipleTransitionsFound st s ts) = "Multiple transitions found for state " ++ st ++ " with input symbol of " ++ show s ++ ". (transitions: " ++ listToString ts ++ ")"
    show AlphabetTooShort = "Empty alphabet."
    show (UndeclaredSymbol var as c) = var ++ " symbol '" ++ [c] ++ "' is not a member of the alphabet " ++ listToString as ++ "."
    show (UndeclaredState var ss st) = var ++ " state " ++ st ++ " is not a member of the machine states " ++ setOfStatesToString ss ++ "."

setOfStatesToString :: SetOfStates -> String
setOfStatesToString xs = "{" ++ body ++ "}"
    where body = List.intercalate ", " $ toList xs

listToString :: Show a => [a] -> String
listToString xs = "{" ++ body ++ "}"
    where body = List.intercalate ", " $ List.map show xs

growTape :: Blank -> (Int, Tape) -> (Int, Tape)
growTape blank (index, tape) = (n, fs)
    where lastIndex = Seq.length tape - 1
          mustGrowLeft = index < 0
          mustGrowRight = index >= lastIndex
          n = if mustGrowLeft then 0 else index
          fs = if mustGrowLeft
                    then Seq.replicate (abs index) blank >< tape
                    else if mustGrowRight
                            then tape >< Seq.replicate (index - lastIndex) blank
                            else tape

mkTape :: Blank -> String -> Tape
mkTape b xs = Seq.fromList xs |> b

mkUniversalMachine :: Machine -> String -> Int -> UniversalMachine
mkUniversalMachine m xs i = UM ts m n $ initialState m
    where b = blank m
          (n, ts) = growTape b (i, mkTape b xs)

isOutOfBound :: Int -> Tape -> Bool
isOutOfBound i tape
    | i < 0 = True
    | i > Seq.length tape = True
    | otherwise = False

readSymbol :: Int -> Tape -> Either RunTimeError Symbol
readSymbol i tape
    | isOutOfBound i tape = Left $ TapeIndexOutOfBound i
    | otherwise = Right $ Seq.index tape i

writeSymbol :: Int -> Symbol -> Tape -> Either RunTimeError Tape
writeSymbol i symbol tape
    | isOutOfBound i tape = Left $ TapeIndexOutOfBound i
    | otherwise = Right $ Seq.update i symbol tape

findTransition :: State -> Symbol -> [Transition] -> Either RunTimeError Transition
findTransition state symbol transitions = do
    let ts = List.filter (transitionFilter state symbol) transitions
    if List.null ts
        then Left $ NoSuchTransition state symbol
        else if List.length ts > 1
                then Left $ MultipleTransitionsFound state symbol ts
                else Right $ head ts

transitionFilter :: State -> Symbol -> Transition -> Bool
transitionFilter state symbol t = state == transitionState && symbol == transitionSymbol
    where transitionState = currentState t
          transitionSymbol = inputSymbol t

updateIndex :: Int -> Shift -> Int
updateIndex n ShiftLeft = n - 1
updateIndex n ShiftRight = n + 1

validate :: Machine -> Either RunTimeError Machine
validate um =   checkAlphabetSize um
            >>= checkBlank
            >>= checkInitialState
            >>= checkInitialState
            >>= checkFinalStates
            >>= checkTransitions

checkSymbol :: Location -> Set.Set Symbol -> Symbol -> Either RunTimeError Symbol
checkSymbol location symbols symbol = do
    if Set.member symbol symbols
        then Right symbol
        else Left $ UndeclaredSymbol location (toList symbols) symbol

checkState :: Location -> SetOfStates -> State -> Either RunTimeError State
checkState location ss m = if Set.member m ss then Right m else Left err
    where err = UndeclaredState location ss m

checkTransition :: Set.Set Symbol -> SetOfStates -> Transition -> Either RunTimeError Transition
checkTransition symbols states transition = do
    checkState "Transition: current" states $ currentState transition
    checkSymbol "Transition: input symbol" symbols $ inputSymbol transition
    checkSymbol "Transition: output symbol" symbols $ outputSymbol transition
    checkState "Transition: new" states $ newState transition
    Right transition

checkAlphabetSize :: Machine -> Either RunTimeError Machine
checkAlphabetSize um = if validSize then Right um else Left AlphabetTooShort
    where size = Set.size $ alphabet um
          validSize = size > 0

checkBlank :: Machine -> Either RunTimeError Machine
checkBlank um = checkSymbol "Blank" as b >> return um
    where b = blank um
          as = alphabet um

checkInitialState :: Machine -> Either RunTimeError Machine
checkInitialState um = checkState "Initial" ss q0 >> return um
    where q0 = initialState um
          ss = states um

checkFinalStates :: Machine -> Either RunTimeError Machine
checkFinalStates m = mapM_ (checkState "Final" ss) fs >> return m
    where fs = toList $ finalStates m
          ss = states m

checkTransitions :: Machine -> Either RunTimeError Machine
checkTransitions m = do
    let ss = alphabet m
        ms = states m
    mapM_ (checkTransition ss ms) $ transitions m
    Right m

step :: UniversalMachine -> Either RunTimeError UniversalMachine
step (UM tape machine index state) = do
    validate machine
    symbolIn <- readSymbol index tape
    transition <- findTransition state symbolIn $ transitions machine
    updatedTape <- writeSymbol index (outputSymbol transition) tape
    let b = blank machine
        updatedIndex = updateIndex index $ shift transition
        nextState = newState transition
        (newIndex, newTape) = growTape b (updatedIndex, updatedTape)
    Right $ UM newTape machine newIndex nextState

isFinal :: UniversalMachine -> Bool
isFinal um = Set.member s $ finalStates m
    where m = machine um
          s = state um

type LogWriterT = WriterT [UniversalMachine] (Either RunTimeError)

runStep :: StateT UniversalMachine (WriterT [UniversalMachine] (Either RunTimeError)) UniversalMachine
runStep = do
    um <- get >>= lift . lift . step
    lift $ tell [um]
    put um
    if isFinal um
        then return um
        else runStep

eval :: UniversalMachine -> Either String [UniversalMachine]
eval um = case execWriterT $ evalStateT runStep um of
            (Left err) -> Left $ show err
            (Right us) -> Right $ um:us

view :: [UniversalMachine] -> IO ()
view [] = putStrLn ""
view ((UM tp _ ix q):us) = do
    let f x y ls = (concatMap (\c -> x:y:c:y:[]) ls) ++ [x]
        tl = Seq.length tp
        xs = List.replicate tl '-'
        ys = f '+' '-' xs
        zs = List.replicate (ix*4) ' '
    putStrLn ""
    putStrLn $ "State: " ++ q
    putStrLn ys
    putStrLn $ f '|' ' ' (toList tp)
    putStrLn ys
    putStrLn $ zs ++ " ^^^"
    view us