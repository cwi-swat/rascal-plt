module Func


// lexical syntax
syntax Ident = lex [a-zA-Z][a-zA-Z0-9]* 
               # [a-zA-Z0-9];

syntax Natural = lex [0-9]+ 
               # [0-9];

syntax LAYOUT = lex [\t-\n\r\ ];

layout LAYOUTLIST = LAYOUT*  # [\t-\n\r\ ] ;

// Context-free syntax

start syntax Prog = prog: Func* ;

syntax Func = func: Ident name "(" {Ident ","}* ")" "=" Exp;

syntax Exp = let: "let" {Binding ","}* "in" Exp "end"
           | cond: "if" Exp "then" Exp "else" Exp "end"
           | bracket "(" Exp ")"
           | var: Ident
           | nat: Natural 
           | call: Ident "(" {Exp ","}* ")"
           | address: "&" Ident
           > deref: "*" Exp 
           > non-assoc (
               left mul: Exp "*" Exp 
             | non-assoc div: Exp "/" Exp
           ) 
           > left (
               left add: Exp "+" Exp 
             | left min: Exp "-" Exp
           )
           >
           non-assoc (
               non-assoc gt: Exp "\>" Exp
             | non-assoc lt:  Exp "\<" Exp
             | non-assoc geq:  Exp "\>=" Exp
             | non-assoc leq:  Exp "\<=" Exp
           )
           >
           right assign: Exp ":=" Exp
           >
           right seq: Exp ";" Exp; 



syntax Binding = binding: Ident "=" Exp;
