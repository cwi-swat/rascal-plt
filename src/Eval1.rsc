module Eval1

// using env, allowing let

import AST;

import List;
import IO;

alias Env = map[str, int];
alias PEnv = map[str, Func];

public int eval(str main, list[int] args, Prog prog) {
  penv = ( f.name: f | f <- prog.funcs );
  f = penv[main];
  env = ( f.formals[i] : args[i] | i <- domain(f.formals) ); 
  return eval(f.body, env, penv);
}

public int eval(Exp exp, Env env, PEnv penv) {
  switch (exp) {
    case nat(int nat):
       return nat;
 
    case var(str name):
       return env[name];
       
    case mul(Exp lhs, Exp rhs): 
      return eval(lhs, env, penv) * eval(rhs, env, penv);
    
    case div(Exp lhs, Exp rhs): 
      return eval(lhs, env, penv) / eval(rhs, env, penv);
    
    case add(Exp lhs, Exp rhs): 
      return eval(lhs, env, penv) + eval(rhs, env, penv);
    
    case min(Exp lhs, Exp rhs): 
      return eval(lhs, env, penv) - eval(rhs, env, penv);
    
    case gt(Exp lhs, Exp rhs): 
      return (eval(lhs, env, penv) > eval(rhs, env, penv)) ? 1 : 0;
    
    case lt(Exp lhs, Exp rhs): 
      return (eval(lhs, env, penv) < eval(rhs, env, penv)) ? 1 : 0;
    
    case geq(Exp lhs, Exp rhs): 
      return (eval(lhs, env, penv) >= eval(rhs, env, penv)) ? 1 : 0;
    
    case leq(Exp lhs, Exp rhs):
      return (eval(lhs, env, penv) <= eval(rhs, env, penv)) ? 1 : 0;
  
  
    case cond(Exp cond, Exp then, Exp otherwise):
       return (eval(cond, env, penv) != 0) ? 
          eval(then, env, penv) : eval(otherwise, env, penv);
          
       
    case call(str name, list[Exp] args): {
       f = penv[name];
       env =  ( f.formals[i]: eval(args[i], env, penv) | i <- domain(f.formals) );
       return eval(f.body, env, penv);
    }
         
    case let(list[Binding] bindings, Exp exp): {
       env += ( b.var : eval(b.exp, env, penv) | b <- bindings );  
       return eval(exp, env, penv);  
    }

    default: throw "Unsupported expression: <exp>";
    
  }
}


