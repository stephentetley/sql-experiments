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
 
namespace Cooper2/SQLTarget {

    use Text/PrettyPrint.Doc;
    use Text/PrettyPrint.{<<>>, <>, <!>, <&>, hang, group, foldDocs, 
        tupled, text, char, int32, squotes, dot, maybeParens};

    use Text/PrettyPrint.Pretty;

    use Cooper2/Common.{TableName, FieldName, Variable};

    /// Fig 5, page 10

    pub enum Query {
        case UnionAll(Query, Query)
        case Select(SelectFromWhere)
    }

    pub enum SelectFromWhere {
        case SelectFromWhere(List[Selector], List[TableAlias], Expr)
    }

    pub enum TableAlias {
        case TableAlias(TableName, Variable)
    }

    pub enum Selector {
        case As(Expr, FieldName)
        case VarAll(Variable)
    }

    pub enum Expr {
        case CaseWhen(Expr, Expr, Expr)
        case Constant(Constant)
        case FieldProjection(Variable, FieldName)
        case And(Expr, Expr)
        case Not(Expr)
        case Exists(Query)
        case Prim(String, List[Expr])
    }

    pub enum Constant {
        case Bool(Bool)
        case String(String)
        case Int(Int32)
        case Null
    }

    instance Pretty[Query] { 
        def pretty(x: Query): Doc = ppQuery(x)
    }

    pub def ppQuery(x: Query): Doc = match x {
        case UnionAll(q1, q2)   => ppQuery(q1) <&> text("UNION ALL") <&> ppQuery(q2)
        case Select(s)          => ppSelectFromWhere(s)
    }

    /// Empty list of table aliases is a special case
    pub def ppSelectFromWhere(x: SelectFromWhere): Doc = match x {
        case SelectFromWhere(ss, Nil, e)    => 
                hang(4, text("SELECT")  <&> vCommaSep(ss, ppSelector))
            <&> hang(4, text("WHERE")   <&> ppExpr(noPrec(), e))

        case SelectFromWhere(ss, ts, e)     => 
                hang(4, text("SELECT")  <&> vCommaSep(ss, ppSelector))
            <&> hang(4, text("FROM")    <&> vCommaSep(ts, ppTableAlias))
            <&> hang(4, text("WHERE")   <&> ppExpr(noPrec(), e))        
    }

    def vCommaSep(xs: List[x], pp: x -> Doc): Doc = 
        foldDocs((x,y) -> x <> char(',') <!> y, List.map(pp, xs))

    pub def ppTableAlias(x: TableAlias): Doc = match x {
        case TableAlias(t, v) => text(t) <<>> text("AS") <<>> text(v)
    }

    pub def ppSelector(x: Selector): Doc = match x {
        case As(e, l)       => ppExpr(noPrec(), e) <<>> text("AS") <<>> text(l)
        case VarAll(v)      => text(v) <> dot() <> char('*')
    }

    /// See Template Haskell for printing with precedence
    /// Precedence support is only a placeholder so far...
    
    instance Pretty[Expr] { 
        def pretty(x: Expr): Doc = ppExpr(noPrec(), x)
    }

    pub def ppExpr(n: Int32, x: Expr): Doc = match x { 
        case CaseWhen(e1, e2, e3)   => 
            maybeParens(n > noPrec(), 
                text("CASE") <<>> text("WHEN") <<>> ppExpr(n, e1) <<>> text("THEN") <<>> ppExpr(n, e2) 
                    <<>> text("ELSE") <<>> ppExpr(n, e3) <<>> text("END")
            )

        case Constant(c)            => ppConstant(n, c)      /// TODO
        case FieldProjection(v, l)  => group(text(v) <> dot() <> text(l))
        case And(e1, e2)            => ppExpr(n, e1) <<>> text("AND") <<>> ppExpr(n, e2)
        case Not(e1)                => text("NOT EXISTS") <<>> ppExpr(n, e1)
        case Exists(q)              => text("EXISTS") <<>> ppQuery(q)
        case Prim(name, es)         => text(name) <<>> tupled(List.map(ppExpr(n), es))
    }

    instance Pretty[Constant] { 
        def pretty(x: Constant): Doc = ppConstant(noPrec(), x)
    }

    pub def ppConstant(n: Int32, x: Constant): Doc = match x {
        case Bool(b)    => if (b) int32(1) else int32(0)
        case String(s)  => squotes(text(s))
        case Int(i)     => maybeParens((n > noPrec() and (i < 0)), int32(i))
        case Null       => text("null")
    }

    def noPrec(): Int32 = 0

}
