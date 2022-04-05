

namespace Reduction {
    
    use Text/PrettyPrint.Doc;
    use Text/PrettyPrint.{<<>>, <>, <!>, <&>, hang, group, foldDocs, 
        parens, text, char, int32, squotes, dot};


    pub enum Term { 
        case Var(String)
        case Lam(String, Term)
        case App(Term, Term)
    }

    pub def freeVars(x: Term): Set[String] = match x { 
        case Var(v)     => Set.singleton(v)
        case Lam(v, e)  => Set.difference(freeVars(e), Set.singleton(v))
        case App(f, a)  => Set.union(freeVars(f), freeVars(a))
    }

    pub def freeIn(v: String, x: Term): Bool = match x { 
        case Var(v1)        => v == v1
        case Lam(v1, e)     => (v == v1) and freeIn(v, e)
        case App(f, a)      => freeIn(v, f) or freeIn(v, a)
    }


    pub def allVars(x: Term): Set[String] = match x { 
        case Var(v)     => Set.singleton(v)
        case Lam(_, e)  => allVars(e)
        case App(f, a)  => Set.union(allVars(f), allVars(a))
    }



    /// See Sestoft lamsml/Lambda.sml

    // s[t/x] in `s` substitute `t` for every free occurence of the variable `x`
    def subst(r: {substitution :: Term, variable :: String}, s: Term, supply: Ref[Int32]): Term & Impure = match s {
        case Var(y)                         => if (r.variable==y) r.substitution else Var(y)
        case App(f, e)                      => App(subst(r, f, supply), subst(r, e, supply))
        case Lam(v, e) if r.variable == v   => Lam(v, e)
        case Lam(v, e)                      => 
            if (freeIn(v, r.substitution) and freeIn(r.variable, e)) {
                let num = deref supply;
                supply := num + 1;
                let y1 = "x.${num}";
                let e1 = subst({substitution = Var(y1), variable = y1}, e, supply);
                let e2 = subst(r, e1, supply); 
                Lam(y1, e2)
            } else {
                let e1 = subst(r, e, supply);
                Lam(v, e1)
            }   
        } 


    pub def cbn(t: Term, supply: Ref[Int32]): Term & Impure = match t {
        case Var(v)                 => Var(v)
        case App(f, a)              => {
            let ans = cbn(f, supply);
            match ans {
                case Lam(x, e)          => cbn(subst({substitution = e, variable = x}, a, supply), supply)
                case e1                 => App(e1, a)
            }
        }
        case Lam(v, a)              => Lam(v, a)
        
    }


    pub def ppTerm(prec: Int32, x: Term): Doc = match x {
        case Var(v)     => text(v)
        case Lam(v, e)  => pparens(prec > 0, char('\\') <> text(v) <> dot() <<>> ppTerm(0, e))
        case App(f, a)  => pparens(prec > 1, ppTerm(1, f) <<>> ppTerm(2, a))
    }

    def pparens(b: Bool, d: Doc): Doc = if (b) parens(d) else d


}
