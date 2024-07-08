

mod Cooper.Phase.Normalize {
    
    use Cooper.Base.Variable
    use Cooper.SourceLang.Term

    // use Cooper1.SourceLang.SrcTerm.{SrcFor, SrcIfte,
    //     SrcTable, SrcBagSingleton, SrcBagEmpty, SrcBagUnion, 
    //     SrcRecord, SrcProj, SrcApp, SrcLam, 
    //     SrcVar, SrcCst, SrcPrim, SrcIsEmpty, 
    //     SrcQuery};
    // use Cooper1.SourceLang.SrcConstant;
    // use Cooper1.SourceLang.SrcConstant.{SrcBool, SrcString, SrcInt};
    // use Cooper1.SourceLang.SrcType;
    // use Cooper1.SourceLang.SrcType.{TyRecord, TyBag,
    //     TyFun, TyBool, TyInt, TyString};

    pub eff GenSym {
        pub def fresh(): Variable
    }


    ///
    /// An IO handler for `GenSym`.
    ///
    pub def ioHandler(f: Unit -> a \ ef + GenSym): a \ ef + IO = region rc {
        let n = ref 0 @ rc;
        try f() with GenSym {
            def fresh(k) = k({let i = deref n; Ref.put(i+1, n); "x.${i}"})
        }
    }


/*
    pub def normalize(t1: Term, supply: Ref[Int32, r]): Term \ r = match t1 {
        // FOR-BETA
        case Term.For(x, Term.BagSingleton(m), n)                       => subst({substitution = m, variable = x}, n, supply)

        // ABS-BETA
        case Term.Apply(Term.Lambda(x, n), m)                                => subst({substitution = m, variable = x}, n, supply)

        // RECORD-BETA
        case Term.Proj(SrcRecord(fs), l)                              => match List.find(xy -> fst(xy) == l, fs) {
            case Some(xy) => snd(xy)
            case None     => bug!("normalize - record missing label `${l}`")
        }

        // FOR-ZERO-L
        case Term.For(_, SrcBagEmpty(ty), _)                          => SrcBagEmpty(ty)

        // FOR-ZERO-R
        case Term.For(_, _, SrcBagEmpty(ty))                          => SrcBagEmpty(ty)

        // FOR-ASSOC
        case Term.For(x, SrcFor(y, l, m), n) if not freeIn(x, n)      => SrcFor(y, l, SrcFor(x, m, n))

        // FOR-UNION-SRC
        case Term.For(x, SrcBagUnion(m1, m2), n)                      => SrcBagUnion(SrcFor(x, m1, n), SrcFor(x, m2, n))

        // FOR-UNION-BODY
        case Term.For(x, m, SrcBagUnion(n1, n2))                      => SrcBagUnion(SrcFor(x, m, n1), SrcFor(x, m, n2))

        // FOR-IF-SRC
        case Term.For(x, SrcIfte(b, m, SrcBagEmpty(ty)), n)           => SrcIfte(b, SrcFor(x, m, n), SrcBagEmpty(ty))

        // APP-IF
        case Term.App(SrcIfte(b, l, l1), m)                           => SrcIfte(b, SrcApp(l, m), SrcApp(l1, m))

        // IF-RECORD (todo)

        // IF-SPLIT (this typechecks but doesn't work as we don't know the proper type of the empty bags)
        // Also - should we have an explicit `not` in the source language?
        case Term.Ifte(_, _, SrcBagEmpty(_))                          => t1
        case Term.Ifte(b, m, n)                                       => 
            SrcBagUnion(SrcIfte(b, m, SrcBagEmpty(TyInt)), SrcIfte(SrcPrim("not", b :: Nil), n, SrcBagEmpty(TyInt)))

        // IF-ZERO
        case Term.Ifte(_, SrcBagEmpty(ty1), SrcBagEmpty(_))           => SrcBagEmpty(ty1)

        // IF-FOR
        case Term.Ifte(b, SrcFor(x, m, n), SrcBagEmpty(ty))           => SrcFor(x, m, SrcIfte(b, n, SrcBagEmpty(ty)))

        // IF-UNION
        case Term.Ifte(b, SrcBagUnion(m, n), SrcBagEmpty(ty))         => 
            SrcBagUnion(SrcIfte(b, m, SrcBagEmpty(ty)), SrcIfte(b, n, SrcBagEmpty(ty)))

        // EMPTY-FLATTEN
        // Does this need a fresh variable for x?
        // What does meta-language parens mean - unit?
        // Need the type as this is conditional on `m` not being relation typed
        case Term.IsEmpty(m)                                          => SrcIsEmpty(SrcFor("x", m, SrcBagEmpty(TyInt)))

        // IGNORE-DB
        case Term.Query(m)                                            => m
        
        case _                                                      => t1
    }

*/


    pub def freeVars(t1: Term): Set[String] = match t1 { 
        case Term.Variable(v)                       => Set.singleton(v)
        case Term.Lambda(v, e)                      => Set.difference(freeVars(e), Set.singleton(v))
        case Term.Apply(f, a)                       => Set.union(freeVars(f), freeVars(a))

        case _                                      => bug!("freeVars incomplete")
    }

    pub def freeIn(v: String, t1: Term): Bool = match t1 { 
        case Term.Variable(v1)                      => v == v1
        case Term.Lambda(v1, e)                     => (v == v1) and freeIn(v, e)
        case Term.Apply(f, a)                       => freeIn(v, f) or freeIn(v, a)

        case _                                      => bug!("freeIn incomplete")
    }


    pub def allVars(t1: Term): Set[String] = match t1 { 
        case Term.Variable(v)                       => Set.singleton(v)
        case Term.Lambda(_, e)                      => allVars(e)
        case Term.Apply(f, a)                       => Set.union(allVars(f), allVars(a))

        case _                                      => bug!("allVars incomplete")
    }



    // See Sestoft lamsml/Lambda.sml

    pub def subst(r: {substitution = Term, variable = String}, s: Term): Term \ GenSym = subst1(r, s, e -> checked_ecast(e))

    // s[t/x] in `s` substitute `t` for every free occurence of the variable `x`
    def subst1(r: {substitution = Term, variable = String}, s: Term, k: Term -> a \ GenSym): a \ GenSym = match s {
        case Term.Variable(y)                           => if (r.variable==y) k(r.substitution) else k(Term.Variable(y))
        case Term.Apply(f, e)                           => subst1(r, f, f1 -> 
                                                            subst1(r, e, e1 -> 
                                                                k(Term.Apply(f1, e1))))
        case Term.Lambda(v, e) if r.variable == v       => k(Term.Lambda(v, e))
        case Term.Lambda(v, e)                          => 
            if (freeIn(v, r.substitution) and freeIn(r.variable, e)) {
                let y1 = do GenSym.fresh();
                subst1({substitution = Term.Variable(y1), variable = y1}, e, e1 -> 
                    subst1(r, e1, e2 -> 
                        k(Term.Lambda(y1, e2))))
            } else {
                subst1(r, e, e1 -> 
                    k(Term.Lambda(v, e1)))
            } 

        case _                                      => bug!("subst incomplete")
    } 



}
