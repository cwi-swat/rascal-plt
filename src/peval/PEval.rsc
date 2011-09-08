module peval::PEval

import peval::AST;
import peval::Eval;

import List;

public str exponent = "exp(x, 4) in 
  'fun exp(x, n) = 
  '  if (= n 0) then 
  '    1 
  '  else 
  '    (* x exp(x, (- n 1))) 
  '  end;";
  
public str exponentSmart = "exp(x, 5) in
  'fun exp(x, n) =
  '  if (= n 0) then
  '    1
  '  else
  '    if (= (% n 2) 0) then
  '       square(exp(x, (/ n 2)))
  '    else
  '       (* x exp(x, (- n 1)))
  '    end
  '  end;
  'fun square(x) = (* x x);";  
  

public Prog pevalMain(Prog prog) {
  Expr peval(const(v), Env env) = const(v);
  
  Expr peval(var(x), Env env) =  ((env[x])?) ? const(env[x]) : var(x);
  
  Expr peval(prim(op, args), Env env) {
    args2 = [ peval(a, env) | a <- args ];
    if (all(a <- args2 && a is const)) 
      return const(evalPrim(op, [v | const(Val v) <- args2]));
    return prim(op, args2);
  }
  
  Expr peval(ifte(c, b1, b2), Env env) {
    c2 = peval(c, env);
    switch (c2) {
    case const(bval(true)): return peval(b1, env);
    case const(bval(false)): return peval(b2, env);
    }
    return ifte(c2, peval(b1, env), peval(b2, env));  
  } 
  
  rel[str, str] memo = {};   
  
  Expr peval(apply(fn, args), Env env) {
    args2 = [ peval(a, env) | a <- args ];
    f = lookup(fn, prog);
    <dynf, dyna> = <[], []>;
    env = ();
    fn2 = fn;
    for (i <- domain(f.formals))
      if (const(v) := args2[i]) { 
        env[f.formals[i]] = v; // store it in env for pevaling body
        fn2 += "_<f.formals[i]>_<v.n>"; // todo: only works for ivals now
      }
      else {
        dynf += [f.formals[i]]; // a dynamic formal param
        dyna += [args2[i]]; // the residual dynamic argument
      }
      
    if (dynf == []) // no dynamic vars means just evaluate the body
      return peval(f.body, env);
    
    if (env == ()) // no static args means make residual call to f
      return apply(fn, args2);
    
    if (<fn2, fn> <- memo) // f already specialized for the same static args
      return apply(fn2, dyna);
      
    memo += {<fn2, fn>};
    prog.funcs += [funcDef(fn2, dynf, peval(f.body, env))];
    return apply(fn2, dyna);
  }
  
  Env env = ();
  prog.expr = peval(prog.expr, env);
  return prog; 
} 





