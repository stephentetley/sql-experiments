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


namespace Cooper2/Monad {

    use Cooper2/Common.{Variable};
    use Cooper2/SourceLanguage.{Term};
    use Cooper2/SourceLanguage.Term.{For, IfThenElse, Table, 
        BagSingleton, BagEmpty, BagUnion, Record, 
        FieldProjection, Apply, Lambda, Variable, Constant,
        Prim, Empty, Query};

    type alias Substs = Map[Variable, Term]

    pub opaque type Mon[a] = Substs -> Result[a, String]

    pub def freevars(t: Term): Set[Variable] = freevarsHelper(t, s -> s)

    def freevarsHelper(t: Term, k: Set[Variable] -> Set[Variable]): Set[Variable] = match t {
        case Variable(x)    => k(Set.singleton(x))
        case Constant(_)    => k(Set.empty())
        case _ => k(Set.empty())
    }
 
}