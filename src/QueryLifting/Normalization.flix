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

namespace QueryLifting/Normalization {


    /// page 8 (Source Language)
    /// TODO - can comprehensions be nested properly...
    pub enum Term {
        case Var(String)
        case Table(String)
        case PrimOp(String, List[Term])
        case Record(List[(String, Term)])
        case Proj(Term, String)
        case Lam(String, Term)
        case App(Term, Term)
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

    pub enum Generator {
        case Generator(String, Term)
    }

    pub def norm(t1: Term): Term = match t1 {

        case App(Lam(x, m), n)                                      => subst({substitution = n, variable = x}, m)

        case Proj(Record(fs), l)                                    => 
            match List.find(lm -> fst(lm) == l, fs) {
                case Some((_, m))                                   => m
                case None                                           => t1
            }

        case SetComprehension(SetEmpty, _)                          => SetEmpty

        case SetComprehension(_, Generator(_, SetEmpty))            => SetEmpty


        case WhereSet(m, PrimOp("true", Nil))                       => m
        
        case WhereSet(m, PrimOp("false", Nil))                      => m

        case WhereSet(BagEmpty, _)                                  => BagEmpty


        case WhereBag(m, PrimOp("true", Nil))                       => m
        
        case WhereBag(m, PrimOp("false", Nil))                      => m

        case _                                                      => t1
    }


    def subst(_: {substitution :: Term, variable :: String}, m: Term): Term = m /// TODO


}