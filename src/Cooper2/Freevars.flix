/*
 * Copyright 2020 Stephen Tetley
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


namespace Cooper2/Freevars {

    use Cooper2/Common.{Variable};
    use Cooper2/SourceLanguage.{Term};
    use Cooper2/SourceLanguage.Term.{For, IfThenElse, Table, 
        BagSingleton, BagEmpty, BagUnion, Record, 
        FieldProjection, Apply, Lambda, Variable, Constant,
        Prim, Empty, Query};


    pub def freevars(t: Term): Set[Variable] = freevarsHelper(t, s -> s)

    def freevarsHelper(t: Term, k: Set[Variable] -> Set[Variable]): Set[Variable] = match t {
        
        /// Presumably this follows "let" in usual free variable calculation and eliminates x in t2?
        case For(x, t1, t2)  => 
            freevarsHelper(t1, s1 -> 
                freevarsHelper(t2, s2 -> 
                    k(Set.union(s1, Set.delete(x, s2)))))

        case IfThenElse(t1, t2, t3)     =>
            freevarsHelper(t1, s1 -> 
                freevarsHelper(t2, s2 -> 
                    freevarsHelper(t3, s3 -> 
                        k(Set.union(s1, Set.union(s2, s3))))))

        case BagSingleton(t1)           => freevarsHelper(t1, s1 -> k(s1))
        
        case BagEmpty                   => k(Set.empty())

        case BagUnion(t1, t2)           => 
            freevarsHelper(t1, s1 -> 
                freevarsHelper(t2, s2 -> 
                    k(Set.union(s1, s2))))

        case Record(_)                  => k(Set.empty())

        case FieldProjection(t1, _)     => freevarsHelper(t1, s1 -> k(s1))

        case Table(_, _)                => k(Set.empty())

        case Apply(t1, t2)              => 
            freevarsHelper(t1, s1 -> 
                freevarsHelper(t2, s2 -> 
                    k(Set.union(s1, s2))))
        
        case Lambda(x, t1)              => 
            freevarsHelper(t1, s1 -> 
                k(Set.delete(x, s1)))
                    
        case Variable(x)                => k(Set.singleton(x))
        case Constant(_)                => k(Set.empty())
        case Prim(_, ts)                => freevarsListHelper(ts, k)
        case Empty(t1)                  => freevarsHelper(t1, s1 -> k(s1))
        case Query(t1)                  => freevarsHelper(t1, s1 -> k(s1))
    }

    def freevarsListHelper(ts: List[Term], k: Set[Variable] -> Set[Variable]): Set[Variable] = match ts { 
        case t1 :: rest => 
            freevarsHelper(t1, s1 -> 
                freevarsListHelper(rest, s2 -> 
                    k(Set.union(s1, s2))))
        case Nil        => k(Set.empty())
    }
 
}