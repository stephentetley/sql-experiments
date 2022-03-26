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


/// Derived from Lennart Augustsson's Lambda-calculus cooked four ways


namespace Substitution/Term {

    use Text/PrettyPrint.Doc;
    use Text/PrettyPrint.{<<>>, <>, <!>, <&>, hang, group, foldDocs, 
        parens, text, char, int32, squotes, dot};

    pub type alias Identifier = String

    pub enum Term[v] { 
        case Var(v)
        case Lam(v, Term[v])
        case App(Term[v], Term[v])
    }

    pub def freeVars(x: Term[v]): Set[v] with Order[v] = match x { 
        case Var(v)     => Set.singleton(v)
        case Lam(v, e)  => Set.difference(freeVars(e), Set.singleton(v))
        case App(f, a)  => Set.union(freeVars(f), freeVars(a))
    }

    pub def allVars(x: Term[v]): Set[v] with Order[v] = match x { 
        case Var(v)     => Set.singleton(v)
        case Lam(_, e)  => allVars(e)
        case App(f, a)  => Set.union(allVars(f), allVars(a))
    }


    pub def ppTermWith(prec: Int32, x: Term[v], ppV: v -> Doc): Doc = match x {
        case Var(v)     => ppV(v)
        case Lam(v, e)  => pparens(prec > 0, char('\\') <> ppV(v) <> dot() <> ppTermWith(0, e, ppV))
        case App(f, a)  => pparens(prec > 1, ppTermWith(1, f, ppV) <<>> ppTermWith(2, a, ppV))
    }

    def pparens(b: Bool, d: Doc): Doc = if (b) parens(d) else d

    
}

namespace Substitution/IdInt {

    use Text/PrettyPrint.Doc;
    use Text/PrettyPrint.{text};

    use Common.StateMonad;
    use Substitution/Term.Term.{Var, Lam, App};
    use Substitution/Term.{freeVars, allVars};

    pub type alias IdInt = Int32

    pub type alias Term0 = Substitution/Term.Term[String]
    pub type alias Term = Substitution/Term.Term[IdInt]

    pub def ppTerm(x:Term): Doc = 
        Substitution/Term.ppTermWith(0, x, ppIdInt)

    def ppIdInt(x: IdInt): Doc = 
        if (x < 0)
            text("f${Int32.toString(Int32.abs(x))}")
        else 
            text("x${Int32.toString(x)}")


    pub type alias SM[a] = StateMonad[(Int32, Map[String, IdInt]), a]

    pub def toIdInt(x: Term0): Term = 
        let vs = Set.toList(freeVars(x));
        let (_, fvmap) = List.foldRight((v, xy) -> let (i, m) = xy; (i + 1, Map.insert(v, 0 - i, m)), (1, Map.empty()), vs);
        Common/StateMonad.evalState(conv(x), (0, fvmap))


    def convVar(v: String): SM[IdInt] = 
        use Common/StateMonad.{flatMap, return, get, put};
        let* (i, m) = get();
        match Map.get(v, m) {
            case None => {
                let* _ = put((i + 1, Map.insert(v, i, m)));
                return(i)

            }
            case Some(ii) => return(ii)
        }

    def conv(x : Term0): SM[Term] = 
        use Common/StateMonad.{liftM, liftM2};
        match x {
            case Var(v) => liftM(Var, convVar(v))
            case Lam(v, e) => liftM2((x, y) -> Lam(x, y), convVar(v), conv(e))
            case App(f, a) => liftM2((x, y) -> App(x, y), conv(f), conv(a))
        }

    pub def whnf(x: Term): Term = match x { 
        case Var(_) => x
        case Lam(_, _) => x
        case App(f, a) => match whnf(f) {
            case Lam(v, b) => whnf(subst(v, a, b))
            case f1 => App(f1, a)
        }
    }


    def subst(x: IdInt, s: Term, b: Term): Term = 
        let fvs = freeVars(s);
        let vs  = Set.union(fvs, allVars(b));
        match b { 
            case Var(v) => if (v == x) s else b
            case Lam(v, e1) => 
                if (v == x)
                    b
                else {
                    let v1 = newId(vs);
                    let e2 = subst(v, Var(v1), e1);
                    if (Set.memberOf(v, fvs))
                        Lam(v1, subst(x, s, e2))
                    else
                        Lam(v, subst(x, s, e1))
                }
            case App(f, a) => App(subst(x, s, f), subst(x, s, a))
        }

    def newId(vs: Set[IdInt]): IdInt = newIdHelper(vs, 0)

    def newIdHelper(vs: Set[IdInt], v: IdInt): IdInt = 
        if (Set.memberOf(v, vs))
            newIdHelper(vs, v + 1)
        else
            v

}