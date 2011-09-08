module peval::AST

import peval::Syntax;
import ParseTree;
import List;

data Prog = prog(Expr expr, list[FuncDef] funcs);


data FuncDef = funcDef(str name, list[str] formals, Expr body);

data Val
  = ival(int n)
  | bval(bool b)
  | pair(Val first, Val second)
  | lst(list[Val] vals)
  ;


data Expr
  = const(Val val)
  | var(str name)
  | prim(Op op, list[Expr] args)
  | ifte(Expr cond, Expr body, Expr elseBody)
  | apply(str func, list[Expr] args)
  ; 


data Op 
  = equal() | add() | sub() | mul() | mkPair() | cons() | fst() | snd() | null() | head() | tail()
  | div() | mod();


public str pp(prog(e, fs)) = "<pp(e)> in" + ("" | "<it>\n<pp(f)>" | f <- fs ); 
public str pp(funcDef(n, fs, b)) = "fun <n>(<intercalate(", ", fs)>) = <pp(b)>;";
public str pp(ival(n)) = "<n>";
public str pp(bval(b)) = "<b>";
public str pp(pair(x, y)) = "\<<pp(x)>, <pp(y)>\>";
public str pp(lst(vs)) = "[<intercalate(", ", [ pp(v) | v <- vs])>]";
public str pp(const(v)) = pp(v);
public str pp(prim(o, as)) = "(<pp(o)> <intercalate(" ", [ pp(a) | a <- as ])>)";
public str pp(ifte(c, b1, b2)) = "if <pp(c)> then
                                 '  <pp(b1)>
                                 'else
                                 '  <pp(b2)>
                                 'end";
public str pp(apply(f, as)) = "<f>(<intercalate(", ", [ pp(a) | a <- as ])>)";
public str pp(var(n)) = n;

// TODO: finish
public str pp(equal()) = "=";
public str pp(add()) = "+";
public str pp(sub()) = "-";
public str pp(mul()) = "*";
public str pp(div()) = "/";
public str pp(mod()) = "%";



public FuncDef lookup(str name, Prog p) = head([ f | f:funcDef(name, _, _) <- p.funcs ]);

public peval::AST::Prog ast(str s) = implode(#peval::AST::Prog, parse(s));
