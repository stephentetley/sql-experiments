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

mod QueryLifting.Normalization {


    /// page 8 (Source Language)
    /// TODO - can comprehensions be nested properly...
    pub enum Term with Eq, ToString {
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

    pub enum Generator with Eq, ToString {
        case Generator(String, Term)
    }

    pub def normalize(t1: Term): Term = match t1 {

        case Term.App(Term.Lam(_, _), _)                                      => bug!("todo") // subst({substitution = n, variable = x}, m)

        case Term.Proj(Term.Record(fs), l)                                    => 
            match List.find(lm -> fst(lm) == l, fs) {
                case Some((_, m))                                   => m
                case None                                           => t1
            }

        case Term.SetComprehension(Term.SetEmpty, _)                          => Term.SetEmpty

        case Term.SetComprehension(_, Generator.Generator(_, Term.SetEmpty))            => Term.SetEmpty


        case Term.WhereSet(m, Term.PrimOp("true", Nil))                       => m
        
        case Term.WhereSet(m, Term.PrimOp("false", Nil))                      => m

        case Term.WhereSet(Term.BagEmpty, _)                                  => Term.BagEmpty


        case Term.WhereBag(m, Term.PrimOp("true", Nil))                       => m
        
        case Term.WhereBag(m, Term.PrimOp("false", Nil))                      => m

        case _                                                      => t1
    }



}