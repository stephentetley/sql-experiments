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



namespace Subst2/Alpha {

    use Functor.{<$>};
    use Applicative.{point};
    use Applicative.{<*>};

    use Subst2/Syntax.Term;
    use Subst2/Syntax.Term.{Var, Cst, Lam, App};

    use Subst2/Mon.Mon;
    use Subst2/Mon.{flatMap, find, local, genId};

    def localBinding(v: String, v1: String, ma: Mon[ka, a]): Mon[ka, a] = 
        local(Map.insert(v, v1), ma)


    pub def ac(t: Term): Mon[ka, Term] = match t {
        case Var(v)             => (v1 -> Var(v1)) <$> find(v)
        case Cst(c)             => point(Cst(c))
        case App(x, y)          => ((t1, t2) -> App(t1, t2)) <$> ac(x) <*> ac(y)
        case Lam(v, e)          => {
            let* v1 = genId(v);
            let* e1 = localBinding(v, v1, ac(e));
            point(Lam(v1, e1))
        }
    }


        



}
