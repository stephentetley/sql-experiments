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


    use Cooper2/SourceLanguage.{SLTerm, SLVariable};
    use Cooper2/SourceLanguage.SLTerm.{SLFor, SLIfThenElse, SLTable, 
        SLBagSingleton, SLBagEmpty, SLBagUnion, SLRecord, 
        SLFieldProjection, SLApply, SLLambda, SLVariable, SLConstant,
        SLPrim, SLEmpty, SLQuery};


    pub def freevars(t: SLTerm): Set[SLVariable] = freevarsHelper(t, s -> s)

    def freevarsHelper(t: SLTerm, k: Set[SLVariable] -> Set[SLVariable]): Set[SLVariable] = match t {
        
        /// Presumably this follows "let" in usual free variable calculation and eliminates x in t2?
        case SLFor(x, t1, t2)  => 
            freevarsHelper(t1, s1 -> 
                freevarsHelper(t2, s2 -> 
                    k(Set.union(s1, Set.remove(x, s2)))))

        case SLIfThenElse(t1, t2, t3)     =>
            freevarsHelper(t1, s1 -> 
                freevarsHelper(t2, s2 -> 
                    freevarsHelper(t3, s3 -> 
                        k(Set.union(s1, Set.union(s2, s3))))))

        case SLBagSingleton(t1)           => freevarsHelper(t1, s1 -> k(s1))
        
        case SLBagEmpty                   => k(Set.empty())

        case SLBagUnion(t1, t2)           => 
            freevarsHelper(t1, s1 -> 
                freevarsHelper(t2, s2 -> 
                    k(Set.union(s1, s2))))

        case SLRecord(_)                  => k(Set.empty())

        case SLFieldProjection(t1, _)     => freevarsHelper(t1, s1 -> k(s1))

        case SLTable(_, _)                => k(Set.empty())

        case SLApply(t1, t2)              => 
            freevarsHelper(t1, s1 -> 
                freevarsHelper(t2, s2 -> 
                    k(Set.union(s1, s2))))
        
        case SLLambda(x, t1)              => 
            freevarsHelper(t1, s1 -> 
                k(Set.remove(x, s1)))
                    
        case SLVariable(x)                => k(Set.singleton(x))
        case SLConstant(_)                => k(Set.empty())
        case SLPrim(_, ts)                => freevarsListHelper(ts, k)
        case SLEmpty(t1)                  => freevarsHelper(t1, s1 -> k(s1))
        case SLQuery(t1)                  => freevarsHelper(t1, s1 -> k(s1))
    }

    def freevarsListHelper(ts: List[SLTerm], k: Set[SLVariable] -> Set[SLVariable]): Set[SLVariable] = match ts { 
        case t1 :: rest => 
            freevarsHelper(t1, s1 -> 
                freevarsListHelper(rest, s2 -> 
                    k(Set.union(s1, s2))))
        case Nil        => k(Set.empty())
    }
 


}