module Eval0

// No let

import AST;
import List;

alias PEnv = map[str, Func];

public value eval(str main, list[value] args, Prog prog) {
  penv = ( f.name: f | f <- prog.funcs );
  f = penv[main];
  return eval(subst(f.body, f.formals, args), penv);
}


public Exp subst(Exp exp, list[str] vars, list[value] values) {
  env = ( vars[i]: values[i] | i <- domain(vars) );
  return visit (exp) {
    case var(str name) => nat(asInt(env[name]))
  };
}

public value eval(Exp exp, PEnv penv) {
  switch (exp) {
    case nat(int nat):
       return nat;
 
    case mul(Exp lhs, Exp rhs): 
      return asInt(eval(lhs, penv)) * asInt(eval(rhs, penv));
    
    case div(Exp lhs, Exp rhs): 
      return asInt(eval(lhs, penv)) / asInt(eval(rhs, penv));
    
    case add(Exp lhs, Exp rhs): 
      return asInt(eval(lhs, penv)) + asInt(eval(rhs, penv));
    
    case min(Exp lhs, Exp rhs): 
      return asInt(eval(lhs, penv)) - asInt(eval(rhs, penv));
    
    case gt(Exp lhs, Exp rhs): 
      return asInt(eval(lhs, penv)) > asInt(eval(rhs, penv));
    
    case lt(Exp lhs, Exp rhs): 
      return asInt(eval(lhs, penv)) < asInt(eval(rhs, penv));
    
    case geq(Exp lhs, Exp rhs): 
      return asInt(eval(lhs, penv)) >= asInt(eval(rhs, penv));
    
    case leq(Exp lhs, Exp rhs):
      return asInt(eval(lhs, penv)) <= asInt(eval(rhs, penv));
  
  
    case cond(Exp cond, Exp then, Exp otherwise):
       return asBool(eval(cond, penv)) ? 
          eval(then, penv) : eval(otherwise, penv);
          
       
    case call(str name, list[Exp] args): {
       f = penv[name];
       return eval(subst(f.body, f.formals, 
           [ eval(a, penv) | a <- args]), penv);
    }
    
    default: throw "Unsupported expression: <exp>";

  }
}


public int asInt(value x) {
  if (int n := x) return n;
  throw "Not an int: <x>";
}

public bool asBool(value x) {
  if (bool b := x) return b;
  throw "Not a bool: <x>";
}