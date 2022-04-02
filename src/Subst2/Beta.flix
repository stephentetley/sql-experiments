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
    use Applicative.{<*>};
    use Monad.{>>=};

    use Subst2/Syntax.Term;
    use Subst2/Syntax.Term.{Var, Cst, Lam, App};

    use Subst2/Mon.Mon;
    use Subst2/Mon.{flatMap, find, freshVar};

    /// pub def bc(t: Term): Mon[ka, Term] = match t {
    ///     case Var(v)             => (v1 -> Var(v1)) <$> find(v)
    ///     case Cst(c)             => point(Cst(c))
    ///     case App(x, y)          => ((t1, t2) -> App(t1, t2)) <$> find(x) <*> ac(y)
    ///     case Lam(v, e)          => {
    ///         let* v1 = genId(v);
    ///         let* e1 = localBinding(v, v1, ac(e));
    ///         point(Lam(v1, e1))
    ///     }
    /// }

    pub def cbn(t: Term): Mon[ka, Term] = match t {
        case Var(v)                 => point(Var(v))
        case Cst(c)                 => point(Cst(c))
        case App(f, a)              => {
            let* ans = cbn(f);
            match ans {
                case Lam(x, e)          => subst(x, a, e) >>= cbn
                case e1                 => point(App(e1, a))
            }
        }
        case Lam(v, a)              => point(Lam(v, a))
        
    }


    pub def subst(x: String, s: Term, t: Term): Mon[ka, Term] = 
        def loop(s1: Term) = match s1 {
            case Var(v)                 => if (v == x) clone(Map.empty(), t) else point(Var(v))
            case Cst(c)                 => point(Cst(c))
            case App(f, a)              => ((f1, a1) -> App(f1, a1)) <$> loop(f) <*> loop(a)
            case Lam(v, a)              => (e1 -> Lam(v, e1)) <$> loop(a)
        };
        loop(s)


    def clone(m: Map[String, String], t: Term): Mon[ka, Term] = match t {
        case Var(v)                 => match Map.get(v, m) {
            case Some(v1) => point(Var(v1))
            case None     => point(Var(v))
        }
        case Cst(c)                 => point(Cst(c))
        case App(f, a)              => ((f1, a1) -> App(f1, a1)) <$> clone(m, f) <*> clone(m, a)
        case Lam(v, e)              => {
            let* v1 = freshVar();
            (e1 -> Lam(v1, e1)) <$> clone(Map.insert(v, v1, m), e)
        }
    }


}
