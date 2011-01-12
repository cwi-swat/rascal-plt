module Eval0

// No let

import AST;
import List;

alias PEnv = map[str, Func];

public value eval(str main, list[int] args, Prog prog) {
  penv = ( f.name: f | f <- prog.funcs );
  f = penv[main];
  return eval(subst(f.body, f.formals, args), penv);
}


public Exp subst(Exp exp, list[str] vars, list[int] values) {
  env = ( vars[i]: values[i] | i <- domain(vars) );
  return visit (exp) {
    case var(str name) => nat(env[name])
  };
}

public int eval(Exp exp, PEnv penv) {
  switch (exp) {
    case nat(int nat):
       return nat;
 
    case mul(Exp lhs, Exp rhs): 
      return eval(lhs, penv) * eval(rhs, penv);
    
    case div(Exp lhs, Exp rhs): 
      return eval(lhs, penv) / eval(rhs, penv);
    
    case add(Exp lhs, Exp rhs): 
      return eval(lhs, penv) + eval(rhs, penv);
    
    case min(Exp lhs, Exp rhs): 
      return eval(lhs, penv) - eval(rhs, penv);
    
    case gt(Exp lhs, Exp rhs): 
      return eval(lhs, penv) > eval(rhs, penv) ? 1 : 0;
    
    case lt(Exp lhs, Exp rhs): 
      return eval(lhs, penv) < eval(rhs, penv) ? 1 : 0;
    
    case geq(Exp lhs, Exp rhs): 
      return eval(lhs, penv) >= eval(rhs, penv) ? 1 : 0;
    
    case leq(Exp lhs, Exp rhs):
      return eval(lhs, penv) <= eval(rhs, penv) ? 1 : 0;
  
  
    case cond(Exp cond, Exp then, Exp otherwise):
       return (eval(cond, penv) != 0) ? 
          eval(then, penv) : eval(otherwise, penv);
          
       
    case call(str name, list[Exp] args): {
       f = penv[name];
       return eval(subst(f.body, f.formals, [ eval(a, penv) | a <- args]), penv);
    }
    
    default: throw "Unsupported expression: <exp>";

  }
}


