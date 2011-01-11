module Eval1

// using env, allowing let

import AST;

import List;
import IO;

alias Env = map[str, value];
alias PEnv = map[str, Func];

public value eval(str main, list[value] args, Prog prog) {
  penv = ( f.name: f | f <- prog.funcs );
  f = penv[main];
  env = ( f.formals[i] : args[i] | i <- domain(f.formals) ); 
  return eval(f.body, env, penv);
}

public value eval(Exp exp, Env env, PEnv penv) {
  switch (exp) {
    case nat(int nat):
       return nat;
 
    case var(str name):
       return env[name];
       


    case mul(Exp lhs, Exp rhs): 
      return asInt(eval(lhs, env, penv)) * asInt(eval(rhs, env, penv));
    
    case div(Exp lhs, Exp rhs): 
      return asInt(eval(lhs, env, penv)) / asInt(eval(rhs, env, penv));
    
    case add(Exp lhs, Exp rhs): 
      return asInt(eval(lhs, env, penv)) + asInt(eval(rhs, env, penv));
    
    case min(Exp lhs, Exp rhs): 
      return asInt(eval(lhs, env, penv)) - asInt(eval(rhs, env, penv));
    
    case gt(Exp lhs, Exp rhs): 
      return asInt(eval(lhs, env, penv)) > asInt(eval(rhs, env, penv));
    
    case lt(Exp lhs, Exp rhs): 
      return asInt(eval(lhs, env, penv)) < asInt(eval(rhs, env, penv));
    
    case geq(Exp lhs, Exp rhs): 
      return asInt(eval(lhs, env, penv)) >= asInt(eval(rhs, env, penv));
    
    case leq(Exp lhs, Exp rhs):
      return asInt(eval(lhs, env, penv)) <= asInt(eval(rhs, env, penv));
  
  
    case cond(Exp cond, Exp then, Exp otherwise):
       return asBool(eval(cond, env, penv)) ? 
          eval(then, env, penv) : eval(otherwise, env, penv);
          
       
    case call(str name, list[Exp] args): {
       f = penv[name];
       return eval(f.body, 
                ( f.formals[i]: eval(args[i], env, penv) | i <- domain(f.formals) ),
                penv);
    }
         
    case let(list[Binding] bindings, Exp exp): 
       return eval(exp, 
                 env + ( b.var : eval(b.exp, env, penv) | b <- bindings ), 
                 penv);  

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
