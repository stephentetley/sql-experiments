/*
 * Copyright 2022 Stephen Tetley
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/// Syntax of the "source language" page 8

mod QueryLifting.Figure4 {


    pub type alias SQL = String

    /// page 8 (Source Language)
    /// TODO - can comprehensions be nested properly or should they be Lists (Nels) ?...
    pub enum Term with Eq, ToString {
        case Var(String)
        case Table(String)
        case PrimOp(String, List[Term])
        case Record(List[(String, Term)])
        case Proj(Term, String)
        case SetEmpty
        case SetSingleton(Term)
        case SetUnion(Term, Term)
        case SetComprehension(Term, Generator)
        case BagEmpty
        case BagSingleton(Term)
        case BagUnion(Term, Term)
        case BagDiff(Term, Term)
        case BagComprehension(Term, Generator)
        case BagToSet(Term)
        case SetToBag(Term)
        case WhereSet(Term, Term)
        case WhereBag(Term, Term)
        case IsEmptySet(Term)
        case IsEmptyBag(Term)
    }

    pub enum Generator with Eq, ToString {
        case Generator(String, Term)
    }

    /// Comprehensions are treated like lambdas...
    pub def freevars(term: Term): Set[String] = 
        def loop(x, k) = match x {
            case Term.Var(v)                     => k(Set.singleton(v))
            case Term.Table(_)                   => k(Set.empty())
            case Term.PrimOp(_, ts)              => Monoid.fold(List.map(freevars, ts))
            case Term.Record(fs)                 => Monoid.fold(List.map(snd >> freevars, fs))
            case Term.Proj(t, _)                 => loop(t, k)
            case Term.SetEmpty                   => k(Set.empty())
            case Term.SetSingleton(q)            => loop(q, k)
            case Term.SetUnion(q1, q2)           => loop(q1, ks1 ->
                                                        loop(q2, ks2 -> 
                                                            k(Set.union(ks1, ks2))))
            case Term.SetComprehension(_, _)     => bug!("missing")
            case Term.BagEmpty                   => k(Set.empty())
            case Term.BagSingleton(r)            => loop(r, k)
            case Term.BagUnion(r1, r2)           => loop(r1, ks1 ->
                                                        loop(r2, ks2 -> 
                                                            k(Set.union(ks1, ks2))))
            case Term.BagDiff(r1, r2)            => loop(r1, ks1 ->
                                                        loop(r2, ks2 -> 
                                                            k(Set.union(ks1, ks2))))
            case Term.BagComprehension(_, _)     => bug!("missing")
            case Term.BagToSet(r)                => loop(r, k)
            case Term.SetToBag(q)                => loop(q, k)
            case Term.WhereSet(q1, q2)           => loop(q1, ks1 ->
                                                        loop(q2, ks2 -> 
                                                            k(Set.union(ks1, ks2))))
            case Term.WhereBag(r1, r2)           => loop(r1, ks1 ->
                                                        loop(r2, ks2 -> 
                                                            k(Set.union(ks1, ks2))))
            case Term.IsEmptySet(q)              => loop(q, k)
            case Term.IsEmptyBag(r)              => loop(r, k)
        };
        loop(term, identity)

    pub def toSql(t1: Term): SQL = match t1 {
        case Term.SetEmpty                          => "SELECT 42 WHERE 0 = 1"
        case Term.Proj(Term.Var(x), l)              => x + "." + l
        case Term.Record(rs)                        => List.joinWith(fieldToSql, ", ", rs)
        case Term.IsEmptySet(q)                     => "NOT EXISTS " + toSql(q)
        case Term.SetUnion(q1, q2)                  => toSql(q1) + " UNION " + toSql(q2)
        case Term.Table(t)                          => "SELECT * FROM " + t
        case Term.BagToSet(Term.Table(t))           => "SELECT DISTINCT * FROM " + t
        case Term.BagToSet(Term.BagDiff(r1, r2))    => "SELECT DISTINCT * FROM (" + toSql(r1) + " EXCEPT ALL " + toSql(r2) + ")"
        case Term.BagEmpty                          => "SELECT 42 WHERE 0 = 1"
        case Term.PrimOp(n, args)                   => n + "(" + List.joinWith(toSql, ",", args) + ")"
        case Term.IsEmptyBag(r)                     => "NOT EXISTS " + toSql(r)
        case Term.BagUnion(r1, r2)                  => toSql(r1) + " UNION ALL " + toSql(r2)
        case Term.BagDiff(r1, r2)                   => toSql(r1) + " EXCEPT ALL " + toSql(r2)
        case Term.SetToBag(q)                       => toSql(q)
        case Term.SetComprehension(Term.WhereSet(Term.SetSingleton(m), x), g)     => 
            "SELECT DISTINCT " + toSql(m) + " FROM " + generatorToSql(g) + " WHERE " + toSql(x)
        case Term.BagComprehension(Term.WhereSet(Term.BagSingleton(m), x), g)     => 
            "SELECT " + toSql(m) + " FROM " + generatorToSql(g) + " WHERE " + toSql(x)
        case _                              => bug!("toSql - not normalized")
    }

    def fieldToSql(p: (String, Term)): SQL = 
        let (n, x) = p;
        n + " AS " + toSql(x)

    def generatorToSql(g: Generator): SQL = match g { 
        case Generator.Generator(x, t) if isClosed(x) => "(" + toSql(t) + ") " + x
        case Generator.Generator(x, t)                => "LATERAL (" + toSql(t) + ") " + x
    }


    def isClosed(_: String): Bool = true        // Placeholder


}
