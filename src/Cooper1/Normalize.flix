

namespace Cooper1/Normalize {
    
    use Cooper1/SourceLang.SrcTerm;
    use Cooper1/SourceLang.SrcTerm.{SrcFor, SrcIfThenElse,
        SrcTable, SrcBagSingleton, SrcBagEmpty, SrcBagUnion, 
        SrcRecord, SrcProj, SrcApp, SrcLam, 
        SrcVar, SrcCst, SrcPrim, SrcEmpty, 
        SrcQuery};
    use Cooper1/SourceLang.SrcConstant;
    use Cooper1/SourceLang.SrcConstant.{SrcBool, SrcString, SrcInt};
    use Cooper1/SourceLang.SrcType;
    use Cooper1/SourceLang.SrcType.{TyRecord, TyBag,
        TyFun, TyBool, TyInt, TyString};

    pub def normalize(t1: SrcTerm, supply: Ref[Int32]): SrcTerm & Impure = match t1 {

        /// ABS-BETA
        case SrcApp(SrcLam(x, n), m)                                => subst({substitution = m, variable = x}, n, supply)

        /// APP-IF
        case SrcApp(SrcIfThenElse(b, l, l1), m)                     => SrcIfThenElse(b, SrcApp(l, m), SrcApp(l1, m))

        /// IF-ZERO
        case SrcIfThenElse(_, SrcBagEmpty(ty1), SrcBagEmpty(_))     => SrcBagEmpty(ty1)

        /// IGNORE-DB
        case SrcQuery(m)                                            => m
        
        case _                                                      => bug!("normalize incomplete")
    }



    pub def freeVars(t1: SrcTerm): Set[String] = match t1 { 
        case SrcVar(v)                              => Set.singleton(v)
        case SrcLam(v, e)                           => Set.difference(freeVars(e), Set.singleton(v))
        case SrcApp(f, a)                           => Set.union(freeVars(f), freeVars(a))

        case _                                      => bug!("freeVars incomplete")
    }

    pub def freeIn(v: String, t1: SrcTerm): Bool = match t1 { 
        case SrcVar(v1)                             => v == v1
        case SrcLam(v1, e)                          => (v == v1) and freeIn(v, e)
        case SrcApp(f, a)                           => freeIn(v, f) or freeIn(v, a)

        case _                                      => bug!("freeIn incomplete")
    }


    pub def allVars(t1: SrcTerm): Set[String] = match t1 { 
        case SrcVar(v)                              => Set.singleton(v)
        case SrcLam(_, e)                           => allVars(e)
        case SrcApp(f, a)                           => Set.union(allVars(f), allVars(a))

        case _                                      => bug!("allVars incomplete")
    }



    /// See Sestoft lamsml/Lambda.sml

    // s[t/x] in `s` substitute `t` for every free occurence of the variable `x`
    def subst(r: {substitution :: SrcTerm, variable :: String}, s: SrcTerm, supply: Ref[Int32]): SrcTerm & Impure = match s {
        case SrcVar(y)                              => if (r.variable==y) r.substitution else SrcVar(y)
        case SrcApp(f, e)                           => SrcApp(subst(r, f, supply), subst(r, e, supply))
        case SrcLam(v, e) if r.variable == v        => SrcLam(v, e)
        case SrcLam(v, e)                           => 
            if (freeIn(v, r.substitution) and freeIn(r.variable, e)) {
                let num = deref supply;
                supply := num + 1;
                let y1 = "x.${num}";
                let e1 = subst({substitution = SrcVar(y1), variable = y1}, e, supply);
                let e2 = subst(r, e1, supply); 
                SrcLam(y1, e2)
            } else {
                let e1 = subst(r, e, supply);
                SrcLam(v, e1)
            } 

        case _                                      => bug!("subst incomplete")
    } 



}
