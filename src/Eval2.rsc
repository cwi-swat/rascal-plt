module Eval2

// local side effects, returning env

import AST;

import List;
import IO;

alias Env = map[str, value];
alias PEnv = map[str, Func];

alias Result = tuple[Env, value];

public Result eval(str main, list[value] args, Prog prog) {
  penv = ( f.name: f | f <- prog.funcs );
  f = penv[main];
  env = ( f.formals[i] : args[i] | i <- domain(f.formals) ); 
  return eval(f.body, env, penv);
}

public Result eval(Exp exp, Env env, PEnv penv) {
  switch (exp) {
    case nat(int nat):
       return <env, nat>;
 
    case var(str name):
       return <env, env[name]>;
       
       
    case mul(Exp lhs, Exp rhs): {
      <env, x> = eval(lhs, env, penv);
      <env, y> = eval(rhs, env, penv);
      return <env, asInt(x) * asInt(y)>;
    } 
      
    case div(Exp lhs, Exp rhs): {
      <env, x> = eval(lhs, env, penv);
      <env, y> = eval(rhs, env, penv);
      return <env, asInt(x) / asInt(y)>;
    } 
      
    case add(Exp lhs, Exp rhs): {
      <env, x> = eval(lhs, env, penv);
      <env, y> = eval(rhs, env, penv);
      return <env, asInt(x) + asInt(y)>;
    } 
      
    case min(Exp lhs, Exp rhs): {
      <env, x> = eval(lhs, env, penv);
      <env, y> = eval(rhs, env, penv);
      return <env, asInt(x) - asInt(y)>;
    } 
      
    case gt(Exp lhs, Exp rhs): {
      <env, x> = eval(lhs, env, penv);
      <env, y> = eval(rhs, env, penv);
      return <env, asInt(x) > asInt(y)>;
    } 
      
    case lt(Exp lhs, Exp rhs): {
      <env, x> = eval(lhs, env, penv);
      <env, y> = eval(rhs, env, penv);
      return <env, asInt(x) < asInt(y)>;
    } 
      
    case geq(Exp lhs, Exp rhs): {
      <env, x> = eval(lhs, env, penv);
      <env, y> = eval(rhs, env, penv);
      return <env, asInt(x) >= asInt(y)>;
    } 
      
    case leq(Exp lhs, Exp rhs): {
      <env, x> = eval(lhs, env, penv);
      <env, y> = eval(rhs, env, penv);
      return <env, asInt(x) <= asInt(y)>;
    } 
  
    case cond(Exp cond, Exp then, Exp otherwise): {
      <env, c> = eval(cond, env, penv);
      return asBool(c) ? eval(then, env, penv) : eval(otherwise, env, penv);
    }
      
    case call(str name, list[Exp] args): {
       f = penv[name];
       for (i <- domain(f.formals)) {
         <env, v> = eval(args[i], env, penv);
         env[f.formals[i]] = v;
       }
       return eval(f.body, env, penv);
    }
         
    case let(list[Binding] bindings, Exp exp): {
       for (b <- bindings) {
         <env, x> = eval(b.exp, env, penv);
         env[b.var] = x;
       }
       return eval(exp, env, penv);
    } 
    
    case seq(Exp lhs, Exp rhs): {
      <env, _> = eval(lhs, env, penv);
      return eval(rhs, env, penv);
    }
    
    case assign(var(str name), Exp exp): {
      <env, v> = eval(exp, env, penv);
      env[name] = v;
      return <env, v>;
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
