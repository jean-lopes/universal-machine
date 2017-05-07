# Universal machine
Turing machine simulation.

## Requirements
1. [Haskell Stack](www.haskellstack.org)
2. [git](https://git-scm.com/) *optional*

## Download the source files

Without git: Donwload and extract the repository

With git: `$ git clone https://github.com/jean-lopes/universal-machine.git`

## Build & Install

```
$ cd universal-machine
$ stack setup
$ stack build
$ stack install
```

Windows: By default the executable will be located in the following directory:

`C:\Users\<your_user>\AppData\Roaming\local\bin\`

## Arguments
`$ universal-machine (-h|--help)`

`$ universal-machine (-t|--tape STRING) (-m|--machine FILE)`

`FILE` is the path to the file with the `machine data`

## Machine DSL
You can use comments and whitespace anywhere in the `machine data` file.
Commentaries:
1. `//` single-line comment
2. `/*` to open commentary block
3. `*/` to close commentary block

```
symbol ::= char

blank ::= symbol

input_symbol ::= symbol

output_symbol ::= symbol

alphabet ::= string

shift ::= L|R

state ::= (letter|_)(alphaNum|_)*

current_state ::= state

new_state ::= state

states ::= {state(, states)*}

initial_state ::= state

final_states ::= states

transition ::= (current_state, input_symbol, output_symbol, shift, new_state)

transitions ::= {transition(, transition)*}

machine ::= M = (states, alphabet, blank, transitions, initial_state, final_states)
```
Node: this is a minimal and informal DSL

## Machine data example

```
/* this is a block commentary */
M =
(
    //states
    {q0, q1, q2}
    
    //alphabet
    , "01b"
    
    //blank
    , 'b'
    
    //transitions
    , { (q0, '0', '1', R, q1)
      , (q1, '0', '1', R, q1)
      , (q1, '1', '0', R, q1)
      , (q1, 'b', 'b', L, q2) }
      
    //initial state
    , q0
    
    //final states
    , {q2}
)
```

## Usage example
```
$ cd universal-machine
$ universal-machine -t "01" -m machine-examples\example.machine

State: q0
+---+---+---+
| 0 | 1 | b |
+---+---+---+
 ^^^

State: q1
+---+---+---+
| 1 | 1 | b |
+---+---+---+
  ^^^

State: q1
+---+---+---+
| 1 | 0 | b |
+---+---+---+
         ^^^

State: q2
+---+---+---+
| 1 | 0 | b |
+---+---+---+
     ^^^

```
