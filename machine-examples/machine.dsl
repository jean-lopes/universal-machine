symbol ::= char

blank  ::= symbol

alphabet ::= string

identifier ::= (letter|_)(alphaNum|_)*

states ::= {identifier(, identifier)*}

shift ::= L|R

transition ::= (identifier, 'char', 'char', shift, identifier)

transitions ::= {transition(, transition)*}

machine ::= M = (states, alphabet, blank, transitions, identifier, states)