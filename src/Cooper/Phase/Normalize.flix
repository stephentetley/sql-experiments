

mod Cooper.Phase.Normalize {
    
    use Cooper.Base.Variable
    use Cooper.Base.{Type, BaseType}
    use Cooper.SourceLang.{Term, RecordField}
    use Cooper.SourceLang.{recordFieldName, recordFieldTerm}

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



    pub def normalize(term: Term): Term \ GenSym = 
        def loop(t1, k) = match t1 {
            // FOR-BETA
            case Term.For(x, Term.BagSingleton(m), n)                       => k(subst({substitution = m, variable = x}, n))

            // ABS-BETA
            case Term.Apply(Term.Lambda(x, n), m)                           => k(subst({substitution = m, variable = x}, n))

            // RECORD-BETA
            case Term.FieldProjection(Term.Record(fs), l)                   => 
                match List.find(xy -> recordFieldName(xy) == l, fs) {
                    case Some(xy) => k(recordFieldTerm(xy))
                    case None     => bug!("normalize - record missing label `${l}`")
                }

            // FOR-ZERO-L
            case Term.For(_, Term.BagEmpty(ty), _)                          => k(Term.BagEmpty(ty))

            // FOR-ZERO-R
            case Term.For(_, _, Term.BagEmpty(ty))                          => k(Term.BagEmpty(ty))

            // FOR-ASSOC
            case Term.For(x, Term.For(y, l, m), n) if not freeIn(x, n)      => k(Term.For(y, l, Term.For(x, m, n)))

            // FOR-UNION-SRC
            case Term.For(x, Term.BagUnion(m1, m2), n)                      => k(Term.BagUnion(Term.For(x, m1, n), Term.For(x, m2, n)))

            // FOR-UNION-BODY
            case Term.For(x, m, Term.BagUnion(n1, n2))                      => k(Term.BagUnion(Term.For(x, m, n1), Term.For(x, m, n2)))

            // FOR-IF-SRC
            case Term.For(x, Term.IfThenElse(b, m, Term.BagEmpty(ty)), n)   => k(Term.IfThenElse(b, Term.For(x, m, n), Term.BagEmpty(ty)))

            // APP-IF
            case Term.Apply(Term.IfThenElse(b, l, l1), m)                   => k(Term.IfThenElse(b, Term.Apply(l, m), Term.Apply(l1, m)))

            // IF-RECORD (todo)

            // IF-SPLIT (this typechecks but doesn't work as we don't know the proper type of the empty bags)
            // Also - should we have an explicit `not` in the source language?
            case Term.IfThenElse(_, _, Term.BagEmpty(_))                        => k(t1)
            case Term.IfThenElse(b, m, n)                                       => 
                k(Term.BagUnion(Term.IfThenElse(b, m, Term.BagEmpty), 
                                Term.IfThenElse(Term.Prim("not", b :: Nil), n, Term.BagEmpty)))

            // IF-ZERO
            case Term.IfThenElse(_, Term.BagEmpty, Term.BagEmpty)               => k(Term.BagEmpty)

            // IF-FOR
            case Term.IfThenElse(b, Term.For(x, m, n), Term.BagEmpty)           => 
                k(Term.For(x, m, Term.IfThenElse(b, n, Term.BagEmpty)))

            // IF-UNION
            case Term.IfThenElse(b, Term.BagUnion(m, n), Term.BagEmpty)         => 
                k(Term.BagUnion(Term.IfThenElse(b, m, Term.BagEmpty), 
                                Term.IfThenElse(b, n, Term.BagEmpty)))

            // EMPTY-FLATTEN
            // Does this need a fresh variable for x?
            // What does meta-language parens mean - unit?
            // Need the type as this is conditional on `m` not being relation typed
            case Term.Empty(m)                                              => 
                k(Term.Empty(Term.For("x", m, Term.BagEmpty)))

            // IGNORE-DB
            case Term.Query(m)                                            => k(m)
   
        case _                                                      => bug!("normalize incomplete")
    
    };
    loop(term, e -> checked_ecast(e))


    pub def freeVars(t: Term): Set[String] = 
        def loop(x, k) = {
            def loopFields(xs, kk) = match xs {
                case Nil                                    => kk(Set.empty())
                case RecordField.RecordField(_, b) :: rs    => loop(b, b1 -> 
                                                                loopFields(rs, rs1 -> 
                                                                    kk(Set.union(b1, rs1))))
            };
            def loopTerms(xs, kk) = match xs {
                case Nil                                    => kk(Set.empty())
                case term :: rs                             => loop(term, term1 -> 
                                                                loopTerms(rs, rs1 -> 
                                                                    kk(Set.union(term1, rs1))))
            };
            match x {
                case Term.For(v, l, m)                      => loop(l, l1 ->
                                                                loop(m, m1 -> 
                                                                    k(Set.union(l1, Set.remove(v, m1)))))
                case Term.IfThenElse(c, tn, el)             => loop(c, c1 ->
                                                                loop(tn, tn1 -> 
                                                                    loop(el, el1 -> 
                                                                        k(Set.union(c1, Set.union(tn1, el1))))))
                case Term.Table(_, _)                       => k(Set.empty())
                case Term.BagSingleton(m)                   => loop(m, k)
                case Term.BagEmpty                          => k(Set.empty())
                case Term.BagUnion(m, n)                    => loop(m, m1 ->
                                                                loop(n, n1 -> 
                                                                    k(Set.union(m1, n1))))
                case Term.Record(fs)                        => loopFields(fs, k)
                case Term.FieldProjection(m, _)             => loop(m, k)
                case Term.Apply(f, a)                       => loop(f, f1 -> 
                                                                loop(a, a1 -> 
                                                                    k(Set.union(f1, a1))))
                case Term.Lambda(v, e)                      => loop(e, e1 -> 
                                                                k(Set.remove(v, e1)))
                case Term.Variable(v)                       => k(Set.singleton(v))
                case Term.Constant(_)                       => k(Set.empty())
                case Term.Prim(_, ts)                       => loopTerms(ts, k)
                case Term.Empty(m)                          => loop(m, k)
                case Term.Query(m)                          => loop(m, k)
            }
        };
        loop(t, identity)

    // Write these in CPS...
    pub def freeIn(v: String, t: Term): Bool = Set.memberOf(v, freeVars(t))
        


    pub def allVars(t: Term): Set[String] = 
        def loop(x, k) = match x { 
            case Term.Variable(v)                       => k(Set.singleton(v))
            case Term.Lambda(_, e)                      => loop(e, k)
            case Term.Apply(f, a)                       => loop(f, f1 -> 
                                                            loop(a, a1 -> 
                                                                k(Set.union(f1, a1))))

            case _                                      => bug!("allVars incomplete")
        };
        loop(t, identity)



    // s[t/x] in `s` substitute `t` for every free occurence of the variable `x`    
    pub def subst(r: {substitution = Term, variable = String}, term: Term): Term \ GenSym =
        def loop(se, x, k) = {
            def loopFields(senv, xs, kk) = match xs {
                case Nil                                    => kk(Nil)
                case RecordField.RecordField(l, b) :: rs    => loop(senv, b, b1 -> 
                                                                loopFields(senv, rs, rs1 -> 
                                                                    kk(RecordField.RecordField(l, b1) :: rs1)))
            };
            def loopTerms(senv, xs, kk) = match xs {
                case Nil                                    => kk(Nil)
                case tt :: rs                               => loop(senv, tt, tt1 -> 
                                                                loopTerms(senv, rs, rs1 -> 
                                                                    kk(tt1 :: rs1)))
            };
            match x {
                case Term.IfThenElse(c, tn, el)             => loop(se, c, c1 ->
                                                                loop(se, tn, tn1 -> 
                                                                    loop(se, el, el1 -> 
                                                                        k(Term.IfThenElse(c1, tn1, el1)))))
                case Term.Table(t, ty)                          => k(Term.Table(t, ty))
                case Term.BagSingleton(m)                       => loop(se, m, m1 -> 
                                                                    k(Term.BagSingleton(m1)))
                case Term.BagEmpty                              => k(Term.BagEmpty)
                case Term.BagUnion(m, n)                        => loop(se, m, m1 ->
                                                                    loop(se, n, n1 -> 
                                                                        k(Term.BagUnion(m1, n1))))
                case Term.Record(fs)                            => loopFields(se, fs, fs1 -> 
                                                                    k(Term.Record(fs1)))
                case Term.FieldProjection(m, l)                 => loop(se, m, m1 -> 
                                                                    k(Term.FieldProjection(m1, l)))
                case Term.Apply(f, e)                           => loop(se, f, f1 -> 
                                                                    loop(se, e, e1 -> 
                                                                        k(Term.Apply(f1, e1))))
                case Term.Lambda(v, e) if r.variable == v       => k(Term.Lambda(v, e))
                // ...
                case Term.Variable(y)                           => if (r.variable==y) k(r.substitution) else k(Term.Variable(y))
                case Term.Constant(c)                           => k(Term.Constant(c))
                case Term.Prim(name, ts)                        => loopTerms(se, ts, ts1 -> 
                                                                    k(Term.Prim(name, ts1)))
                case Term.Empty(m)                              => loop(se, m, m1 -> 
                                                                    k(Term.Empty(m1)))
                case Term.Query(m)                              => loop(se, m, m1 -> 
                                                                    k(Term.Query(m1)))
                case _                                          => bug!("subst incomplete")
            }
        };
        loop(r, term, e -> checked_ecast(e))



}
