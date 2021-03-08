/*
 * Copyright 2021 Stephen Tetley
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

/// Stefano Ceri, Georg Gottlob, Letizia Tanca
/// Logic Programming and Databases

namespace Ceri/Translate {

    use Ceri/Monad.Mon;

    /// Relational Algebra - not a full or correct definition...

    pub enum Formula { 
        case AttributeAttribute
        case AttributeConstant
    }

    pub enum Relation { 
        case Select(Formula, Relation)
        case Project(List[Int32], Relation)      // Capital pi
        case CartesianProduct(Relation, Relation)
        case Table(String)
    }


    /// Syntax p.77...
    pub enum Term {
        case Var(String)
        case Const(Constant)
    }

    instance ToString[Term] {
        def toString(x: Term): String = match x {
            case Var(s) => s
            case Const(x1) => ToString.toString(x1)
        }
    }


    instance Eq[Term] {
        def eq(x: Term, y: Term): Bool = match (x, y) {
            case (Var(s1), Var(s2)) => s1 == s2
            case (Const(c1), Const(c2)) => c1 == c2
            case (_, _) => false
        }

        def neq(x: Term, y: Term): Bool = not (x == y)
    }


    pub enum Constant {
        case String(String)
        case Int(Int32)
    }

    instance ToString[Constant] {
        def toString(x: Constant): String = match x {
            case String(s) => "'${s}'"
            case Int(i) => ToString.toString(i)
        }
    }

    instance Eq[Constant] {
        def eq(x: Constant, y: Constant): Bool = match (x, y) {
            case (String(s1), String(s2)) => s1 == s2
            case (Int(i1), Int(i2)) => i1 == i2
            case (_, _) => false
        }

        def neq(x: Constant, y: Constant): Bool = not (x == y)
    }

    type alias Pred = String

    pub enum Atom {
        case Atom(Pred, List[Term])
    }

    instance Eq[Atom] {
        def eq(x: Atom, y: Atom): Bool = match (x, y) { 
            case (Atom(p1, args1), Atom(p2, args2)) => p1 == p2 and args1 == args2
        }
        
        def neq(x: Atom, y: Atom): Bool = not (x == y) 
    }

    def getPred(x: Atom): Pred = 
        let Atom(p, _) = x;
        p

    def getArgs(x: Atom): List[Term] = 
        let Atom(_, xs) = x;
        xs


    pub enum Clause {
        case Clause(Atom, List[Atom])
    }

    def getLhs(x: Clause): Atom = 
        let Clause(lhs, _) = x;
        lhs

    def getRhs(x: Clause): List[Atom] = 
        let Clause(_, rhs) = x;
        rhs

    pub def occurs(x: Term, rhs: List[Atom]): Bool = 
        let f = q -> List.memberOf(x, getArgs(q));
        List.exists(f, rhs)


    pub def const(x: Term): Bool = match x {
        case Var(_) => false
        case Const(_) => true
    }

    pub def var(x: Term): Bool = not const(x)

    pub def replaceSymbol1(atom: Atom, m: Term, n:Term): Atom =  
        Atom(getPred(atom), replaceFirst(m, n, getArgs(atom)))

    pub def replaceSymbol(xs: List[Atom], m: Term, n:Term): List[Atom] = match xs {
        case Nil => Nil
        case x :: rest => { 
            let x1 = replaceSymbol1(x, m, n);
            if (x1 == x)
                x :: replaceSymbol(rest, m ,n)
            else
                x1 :: rest
        }
    }

    def replaceFirst[a: Eq](m: a, n: a, xs: List[a]): List[a] = match xs { 
        case Nil => Nil
        case x :: rest if x == m => n :: rest
        case x :: rest if x == m => n :: replaceFirst(m, n, rest)

    }

    /// 0 indexed
    pub def corr(ix: Int32, lhs: Atom, rhs: List[Atom]): Int32 = 
        match nth(ix, getArgs(lhs)) {
            case None => -1
            case Some(t) => corrHelper(0, t, rhs)
        }

    def corrHelper(n: Int32, t: Term, xs: List[Atom]): Int32 = match xs {
        case Nil => -1
        case x1 :: rest => {
            let args = getArgs(x1);
            let ix = List.indexOf(t, args);
            if (ix < 0)
                corrHelper(n + List.length(args), t, rest)
            else 
                n + ix
        }
    }
        

    /// 1 indexed
    def nth(ix: Int32, xs: List[a]): Option[a] = 
        if (ix < 1)
            None
        else {
            match xs { 
                case x :: _ if ix == 1 => Some(x)
                case _ :: rest => nth(ix - 1, rest)
                case Nil => None
            }
        }

    def findConst(atoms: List[Term]): Option[(Int32, Term)] = findConstHelper(const, atoms, 0)


    ///
    /// Helper function for `findConst`.
    ///
    def findConstHelper(f: Term -> Bool, xs: List[Term], acc: Int32): Option[(Int32, Term)] = match xs {
        case Nil => None
        case x :: rs => if (f(x)) Some((acc, x)) else findConstHelper(f, rs, acc+1)
    }

    def findMultiple(atoms: List[Term]): Option[((Int32, Term), (Int32, Term))] = findMultipleHelper1(atoms, 0)


    ///
    /// Helper function for `findMultiple`.
    ///
    def findMultipleHelper1(xs: List[Term], acc: Int32): Option[((Int32, Term), (Int32, Term))] = match xs {
        case Nil => None
        case x :: rs => match findMultipleHelper2(x, rs, acc) {
            case Some((j, alphaj)) => Some(((acc, x), (j, alphaj)))
            case None => findMultipleHelper1(rs, acc + 1)
        }
    }

    def findMultipleHelper2(alphai: Term, xs: List[Term], acc: Int32): Option[(Int32, Term)] = match xs {
        case Nil => None
        case x :: _ if x == alphai => Some((acc, x))
        case _ :: rs => findMultipleHelper2(alphai, rs, acc+1)
    }


    def newvar(): Mon[Term] = 
        use Ceri/Monad.{map, newvar};
        map(Var, newvar())

    def snocEQ(xs: List[Atom], x: Term, alphai: Term): List[Atom] = 
        List.append(xs, Atom("EQ", x :: alphai :: Nil) :: Nil)

    pub def translateC(c: Clause): Mon[Relation] = 
        use Ceri/Monad.{flatMap, point};
        let lhs = getLhs(c);
        let rhs: List[Atom] = getRhs(c);
        let alphas = getArgs(lhs);
        let n = List.length(alphas);
        match findConst(alphas) {
            case Some((_, alphai)) => { 
                let* x = newvar();
                translateC(Clause(replaceSymbol1(lhs, alphai, x), snocEQ(rhs, x, alphai)))
            }
            case None => match findMultiple(alphas) {
                case Some(((_, alphai), (_, alphaj))) => { 
                    let* x = newvar();
                    translateC(Clause(replaceSymbol1(lhs, alphai, x), snocEQ(rhs, x, alphaj)))
                }
                case None => {                    
                    let ixs = List.map(ix1 -> corr(ix1, lhs, rhs), List.range(0, n - 1));
                    let* reln = translateRhs(rhs);
                    point(Project(ixs, reln))
                }
            }
        }

    pub def translateRhs(rhs: List[Atom]): Mon[Relation] = 
        use Ceri/Monad.{flatMap, point};
        let betas = List.flatMap(getArgs, rhs);
        match findConst(betas) {
            case Some((_i, betai)) => { 
                let* x = newvar();
                let* reln = translateRhs(replaceSymbol(rhs, betai, x));
                point(Select(AttributeConstant, reln))
            }
            case None => match findMultiple(betas) {
                case Some(((_i, betai), (_j, _betaj))) => {
                    let* x = newvar();
                    let* reln = translateRhs(replaceSymbol(rhs, betai, x));
                    point(Select(AttributeAttribute, reln))
                }
                case None => point(CartesianProduct(Table("m i=1"), Table("Qi")))
            }
        }

}