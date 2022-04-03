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

namespace QueryLifting/Figure4 {

    use Text/PrettyPrint.Doc;
    use Text/PrettyPrint.{<<>>, <>, <!>, <&>, hang, group, foldDocs, 
        parens, text, char, int32, squotes, dot};

    pub type alias SQL = String

    /// page 8 (Source Language)
    /// TODO - can comprehensions be nested properly...
    pub enum Term {
        case Var(String)
        case Table(String)
        case PrimOp(String, List[Term])
        case Record(List[(String, Term)])
        case Proj(Term, String)
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

    pub enum Generator {
        case Generator(String, Term)
    }

    pub def toSql(t1: Term): SQL = match t1 {
        case SetEmpty                       => "SELECT 42 WHERE 0 = 1"
        case Proj(Var(x), l)                => x + "." + l
        case Record(rs)                     => List.joinWith(fieldToSql, ", ", rs)
        case IsEmptySet(q)                  => "NOT EXISTS " + toSql(q)
        case SetUnion(q1, q2)               => toSql(q1) + " UNION " + toSql(q2)
        case Table(t)                       => "SELECT * FROM " + t
        case BagToSet(Table(t))             => "SELECT DISTINCT * FROM " + t
        case BagToSet(BagDiff(r1, r2))      => "SELECT DISTINCT * FROM (" + toSql(r1) + " EXCEPT ALL " + toSql(r2) + ")"
        case BagEmpty                       => "SELECT 42 WHERE 0 = 1"
        case PrimOp(n, args)                => n + "(" + List.joinWith(toSql, ",", args) + ")"
        case IsEmptyBag(r)                  => "NOT EXISTS " + toSql(r)
        case BagUnion(r1, r2)               => toSql(r1) + " UNION ALL " + toSql(r2)
        case BagDiff(r1, r2)                => toSql(r1) + " EXCEPT ALL " + toSql(r2)
        case SetToBag(q)                    => toSql(q)
        case SetComprehension(WhereSet(SetSingleton(m), x), g)     => 
            "SELECT DISTINCT " + toSql(m) + " FROM " + generatorToSql(g) + " WHERE " + toSql(x)
        case BagComprehension(WhereSet(BagSingleton(m), x), g)     => 
            "SELECT " + toSql(m) + " FROM " + generatorToSql(g) + " WHERE " + toSql(x)
        case _                              => bug!("toSql - not normalized")
    }

    def fieldToSql(p: (String, Term)): SQL = 
        let (n, x) = p;
        n + " AS " + toSql(x)

    def generatorToSql(g: Generator): SQL = match g { 
        case Generator(x, t) if isClosed(x) => "(" + toSql(t) + ") " + x
        case Generator(x, t)                => "LATERAL (" + toSql(t) + ") " + x
    }


    def isClosed(_: String): Bool = true        /// Placeholder

}