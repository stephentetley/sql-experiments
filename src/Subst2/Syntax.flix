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



namespace Subst2/Syntax {

    use Text/PrettyPrint.Doc;
    use Text/PrettyPrint.{<<>>, <>, <!>, <&>, hang, group, foldDocs, 
        parens, text, char, int32, squotes, dot};


    pub enum Term { 
        case Var(String)
        case Cst(Constant)
        case Lam(String, Term)
        case App(Term, Term)
    }

    pub enum Constant {
        case CNone
        case CInt(Int32)
        case CString(String)
    }

    pub def freeVars(x: Term): Set[String] = match x { 
        case Var(v)     => Set.singleton(v)
        case Cst(_)     => Set.empty()
        case Lam(v, e)  => Set.difference(freeVars(e), Set.singleton(v))
        case App(f, a)  => Set.union(freeVars(f), freeVars(a))
    }

    pub def allVars(x: Term): Set[String] = match x { 
        case Var(v)     => Set.singleton(v)
        case Cst(_)     => Set.empty()
        case Lam(_, e)  => allVars(e)
        case App(f, a)  => Set.union(allVars(f), allVars(a))
    }


    pub def ppTerm(prec: Int32, x: Term): Doc = match x {
        case Var(v)     => text(v)
        case Cst(c)     => ppConstant(c)
        case Lam(v, e)  => pparens(prec > 0, char('\\') <> text(v) <> dot() <<>> ppTerm(0, e))
        case App(f, a)  => pparens(prec > 1, ppTerm(1, f) <<>> ppTerm(2, a))
    }

    def ppConstant(x: Constant): Doc = match x {
        case CNone          => text("()")
        case CInt(i)        => int32(i)
        case CString(s)     => squotes(text(s))
    }

    def pparens(b: Bool, d: Doc): Doc = if (b) parens(d) else d

    
}
