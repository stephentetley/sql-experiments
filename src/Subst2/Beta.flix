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



namespace Subst2/Beta {

    use Functor.{<$>};
    use Applicative.{point};

    use Subst2/Syntax.Term;
    use Subst2/Syntax.Term.{Var, Cst, Lam, App};

    use Subst2/Mon.Mon;
    use Subst2/Mon.{find};

    pub def ac(t: Term): Mon[ka, Term] = match t {
        case Var(v)             => (v1 -> Var(v1)) <$> find(v)
        case _                  => point(t)
    }


        



}
