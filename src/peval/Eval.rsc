module peval::Eval

import peval::AST;

public Val evalPrim(fst(), [pair(x, y)]) = x;
public Val evalPrim(snd(), [pair(x, y)]) = y;
public Val evalPrim(Op::head(), [lst([x,_*])]) = x;
public Val evalPrim(Op::tail(), [lst([_,x*])]) = lst(x);
public Val evalPrim(equal(), [x, y]) = bval(x == y);

public Val evalPrim(add(), [ival(x), ival(y)]) = ival(x + y);
public Val evalPrim(sub(), [ival(x), ival(y)]) = ival(x - y);
public Val evalPrim(mul(), [ival(x), ival(y)]) = ival(x * y);
public Val evalPrim(div(), [ival(x), ival(y)]) = ival(x / y);
public Val evalPrim(mod(), [ival(x), ival(y)]) = ival(x % y);

public Val evalPrim(mkPair(), [x, y]) = pair(x, y);
public Val evalPrim(cons(), [x, lst(xs)]) = lst([x, xs]);

alias Env = map[str var, Val val];

public void evalMain(Prog prog) {
  FuncDef lookup(str name) = 
    head([ f | f:funcDef(name, _, _) <- prog.funcs ]);
 
  Val eval(const(v), Env env) = v;
  Val eval(var(x), Env env) =  env[x];
  Val eval(prim(op, args)) = evalPrim(op(), [ eval(a, env) | a <- args ]);
  Val eval(ifte(c, b1, b2), Env env) = bval(true) := eval(c, env) ? eval(b1, env) : eval(b2, env);
  Val eval(apply(func, args)) {
    f = head([ f | f:funcDef(func, _, _) <- prog.funcs ]);
    env = ( f.formals[i]: eval(args[i], env) | i <- domain(fs) );
    return eval(func.body, env);
  }
} 
