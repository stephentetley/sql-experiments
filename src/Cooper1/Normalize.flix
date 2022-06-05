

namespace Cooper1/Normalize {
    
    use Cooper1/SourceLang.SrcTerm;
    use Cooper1/SourceLang.SrcTerm.{SrcFor, SrcIfte,
        SrcTable, SrcBagSingleton, SrcBagEmpty, SrcBagUnion, 
        SrcRecord, SrcProj, SrcApp, SrcLam, 
        SrcVar, SrcCst, SrcPrim, SrcIsEmpty, 
        SrcQuery};
    use Cooper1/SourceLang.SrcConstant;
    use Cooper1/SourceLang.SrcConstant.{SrcBool, SrcString, SrcInt};
    use Cooper1/SourceLang.SrcType;
    use Cooper1/SourceLang.SrcType.{TyRecord, TyBag,
        TyFun, TyBool, TyInt, TyString};

    pub def normalize(t1: SrcTerm, supply: Ref[Int32, r]): SrcTerm \ Write(r) = match t1 {
        /// FOR-BETA
        case SrcFor(x, SrcBagSingleton(m), n)                       => subst({substitution = m, variable = x}, n, supply)

        /// ABS-BETA
        case SrcApp(SrcLam(x, n), m)                                => subst({substitution = m, variable = x}, n, supply)

        /// RECORD-BETA
        case SrcProj(SrcRecord(fs), l)                              => match List.find(xy -> fst(xy) == l, fs) {
            case Some(xy) => snd(xy)
            case None     => bug!("normalize - record missing label `${l}`")
        }

        /// FOR-ZERO-L
        case SrcFor(_, SrcBagEmpty(ty), _)                          => SrcBagEmpty(ty)

        /// FOR-ZERO-R
        case SrcFor(_, _, SrcBagEmpty(ty))                          => SrcBagEmpty(ty)

        /// FOR-ASSOC
        case SrcFor(x, SrcFor(y, l, m), n) if not freeIn(x, n)      => SrcFor(y, l, SrcFor(x, m, n))

        /// FOR-UNION-SRC
        case SrcFor(x, SrcBagUnion(m1, m2), n)                      => SrcBagUnion(SrcFor(x, m1, n), SrcFor(x, m2, n))

        /// FOR-UNION-BODY
        case SrcFor(x, m, SrcBagUnion(n1, n2))                      => SrcBagUnion(SrcFor(x, m, n1), SrcFor(x, m, n2))

        /// FOR-IF-SRC
        case SrcFor(x, SrcIfte(b, m, SrcBagEmpty(ty)), n)           => SrcIfte(b, SrcFor(x, m, n), SrcBagEmpty(ty))

        /// APP-IF
        case SrcApp(SrcIfte(b, l, l1), m)                           => SrcIfte(b, SrcApp(l, m), SrcApp(l1, m))

        /// IF-RECORD (todo)

        /// IF-SPLIT (this typechecks but doesn't work as we don't know the proper type of the empty bags)
        /// Also - should we have an explicit `not` in the source language?
        case SrcIfte(_, _, SrcBagEmpty(_))                          => t1
        case SrcIfte(b, m, n)                                       => 
            SrcBagUnion(SrcIfte(b, m, SrcBagEmpty(TyInt)), SrcIfte(SrcPrim("not", b :: Nil), n, SrcBagEmpty(TyInt)))

        /// IF-ZERO
        case SrcIfte(_, SrcBagEmpty(ty1), SrcBagEmpty(_))           => SrcBagEmpty(ty1)

        /// IF-FOR
        case SrcIfte(b, SrcFor(x, m, n), SrcBagEmpty(ty))           => SrcFor(x, m, SrcIfte(b, n, SrcBagEmpty(ty)))

        /// IF-UNION
        case SrcIfte(b, SrcBagUnion(m, n), SrcBagEmpty(ty))         => 
            SrcBagUnion(SrcIfte(b, m, SrcBagEmpty(ty)), SrcIfte(b, n, SrcBagEmpty(ty)))

        /// EMPTY-FLATTEN
        /// Does this need a fresh variable for x?
        /// What does meta-language parens mean - unit?
        /// Need the type as this is conditional on `m` not being relation typed
        case SrcIsEmpty(m)                                          => SrcIsEmpty(SrcFor("x", m, SrcBagEmpty(TyInt)))

        /// IGNORE-DB
        case SrcQuery(m)                                            => m
        
        case _                                                      => t1
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
    def subst(r: {substitution :: SrcTerm, variable :: String}, s: SrcTerm, supply: Ref[Int32, r]): SrcTerm \ Write(r) = match s {
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
