module Command (
    command
) where
import Text.Parsec hiding (State)
import Text.Parsec.String (Parser)
import UniversalMachine (State, Symbol, Transition)
import UniversalMachine.Parser (machineStateParser, symbolParser, transitionParser)

data Command = Save
             | Load
             | Next
             | Previous
             | Continue
             | AddState State
             | AddSymbol Symbol
             | AddTransition Transition
             | AddFinalState State
             | AddBreakpoint State
             | RemoveState State
             | RemoveSymbol Symbol
             | RemoveTransition Transition
             | RemoveFinalState State
             | RemoveBreakpoint State
             | UpdateBlankSymbol Symbol
             | UpdateInitialState State
             | UpdateTapeAppend Symbol
             | UpdateTapePrepend Symbol
             | UpdateTape Symbol
             | ViewMachine
             | ViewStates
             | ViewAlphabet
             | ViewTransitions
             | ViewInitialState
             | ViewFinalStates
             | ViewBlankSymbol
             | ViewState State
             | ViewBreakpoints
             deriving Show

instructionWithArgs :: String -> Parser a -> (a -> Command) -> Parser Command
instructionWithArgs str p f = try $ string str >> spaces >> p >>= return . f

instruction :: String -> Command -> Parser Command
instruction str cmd = try $ string str >> return cmd

stateInstruction :: String -> (State -> Command) -> Parser Command
stateInstruction str f = instructionWithArgs str machineStateParser f

symbolInstruction :: String -> (Symbol -> Command) -> Parser Command
symbolInstruction str f = instructionWithArgs str symbolParser f

transitionInstruction :: (Transition -> Command) -> Parser Command
transitionInstruction f = instructionWithArgs "transition" transitionParser f

add :: Parser Command
add = string "add" >> spaces >> choice
    [ stateInstruction "state" AddState
    , symbolInstruction "symbol" AddSymbol
    , transitionInstruction AddTransition
    , stateInstruction "final" AddFinalState
    , stateInstruction "breakpoint" AddBreakpoint
    ]

remove :: Parser Command
remove = string "remove" >> spaces >> choice
    [ stateInstruction "state" RemoveState
    , symbolInstruction "symbol" RemoveSymbol
    , transitionInstruction RemoveTransition
    , stateInstruction "final" RemoveFinalState
    , stateInstruction "breakpoint" RemoveBreakpoint
    ]

tape :: Parser Command
tape = string "tape" >> spaces >> choice
    [ symbolInstruction "prepend" UpdateTapePrepend
    , symbolInstruction "append" UpdateTapeAppend
    , symbolParser >>= return . UpdateTape
    ]

update :: Parser Command
update = string "update" >> spaces >> choice
    [ symbolInstruction "blank" UpdateBlankSymbol
    , stateInstruction "initial" UpdateInitialState
    , tape
    ]

view :: Parser Command
view = string "view" >> spaces >> choice
    [ instruction "machine" ViewMachine
    , instruction "states" ViewStates
    , instruction "alphabet" ViewAlphabet
    , instruction "transitions" ViewTransitions
    , instruction "initial" ViewInitialState
    , instruction "finals" ViewFinalStates
    , instruction "blank" ViewBlankSymbol
    , stateInstruction "state" ViewState
    , instruction "breakpoints" ViewBreakpoints
    ]

command :: Parser Command
command = spaces >> choice [ instruction "save" Save
                           , instruction "load" Load
                           , instruction "next" Next
                           , instruction "previous" Previous
                           , instruction "continue" Continue
                           , add
                           , remove
                           , update
                           , view
                           ]
