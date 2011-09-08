module peval::Syntax

import ParseTree;

import lang::std::Id;
import lang::std::Whitespace;
import lang::std::Comment;
import lang::std::Layout;


start syntax Prog = prog: Expr "in" FuncDef*;

syntax FuncDef = funcDef: "fun" Id "(" {Id ","}* ")" "=" Expr ";";

syntax Expr
  = const: Val
  | var: Id
  | prim: "(" Op Expr* ")"
  | ifte: "if" Expr "then" Expr "else" Expr "end"
  | apply: Id "(" {Expr ","}* ")"
  ;

syntax Val
  = ival: Int 
  | bval: Bool
  | pair: "\<" Val "," Val "\>"
  | lst: "[" {Val ","}* "]"
  ;

syntax Op
  = equal: "="
  | add: "+"
  | sub: "-"
  | mul: "*"
  | div: "/"
  | mod: "%"
  | mkPair: "."
  | cons: "cons"
  | fst: "fst"
  | snd: "snd"
  | null: "null"
  | head: "head"
  | tail: "tail"
  ;

lexical Int = [0-9]+ !>> [0-9];

lexical Bool = "true" | "false";


public peval::Syntax::Prog parse(str s) {
  return parse(#peval::Syntax::Prog, s);
}