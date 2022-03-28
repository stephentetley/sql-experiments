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

namespace Cooper2 { 

    use Text/PrettyPrint.Doc;

    pub lawless class Pretty[a] { 
        pub def pretty(x: a): Doc
    }

}

namespace Cooper2/SQLTarget {

    use Text/PrettyPrint.Doc;
    use Text/PrettyPrint.{<<>>, <>, <!>, <&>, hang, group, foldDocs, 
        tupled, text, char, int32, squotes, dot, maybeParens};

    use Cooper2.Pretty;
    /// use Cooper2.Pretty.{pretty};

   
    pub type alias SQLTableName = String       /// t
    pub type alias SQLFieldName = String       /// l
    pub type alias SQLVariable = String        /// x

    /// Fig 5, page 10

    pub enum SQLQuery {
        case SQLUnionAll(SQLQuery, SQLQuery)
        case SQLSelect(SQLSelectFromWhere)
    }

    pub type alias SQLSelectFromWhere = 
        { selectors :: List[SQLSelector]
        , tableAliases :: List[SQLTableAlias]
        , whereExpr :: SQLExpr
        }

    pub type alias SQLTableAlias = 
        { tableName :: SQLTableName
        , variable :: SQLVariable
        }

    pub enum SQLSelector {
        case SQLAs(SQLExpr, SQLFieldName)
        case SQLVarAll(SQLVariable)
    }

    pub enum SQLExpr {
        case SQLCaseWhen(SQLExpr, SQLExpr, SQLExpr)
        case SQLConstant(SQLConstant)
        case SQLFieldProjection(SQLVariable, SQLFieldName)
        case SQLAnd(SQLExpr, SQLExpr)
        case SQLNot(SQLExpr)
        case SQLExists(SQLQuery)
        case SQLPrim(String, List[SQLExpr])
    }

    pub enum SQLConstant {
        case SQLBool(Bool)
        case SQLString(String)
        case SQLInt(Int32)
        case SQLNull
    }

    instance Pretty[SQLQuery] { 
        pub def pretty(x: SQLQuery): Doc = ppQuery(x)
    }

    pub def ppQuery(x: SQLQuery): Doc = match x {
        case SQLUnionAll(q1, q2)   => ppQuery(q1) <&> text("UNION ALL") <&> ppQuery(q2)
        case SQLSelect(s)          => ppSelectFromWhere(s)
    }

    /// Empty list of table aliases is a special case
    pub def ppSelectFromWhere(r: SQLSelectFromWhere): Doc = match r.tableAliases {
        case Nil => 
                hang(4, text("SELECT")  <&> vCommaSepWith(r.selectors, ppSelector))
            <&> hang(4, text("WHERE")   <&> ppSQLExpr(noPrec(), r.whereExpr))

        case ts  => 
                hang(4, text("SELECT")  <&> vCommaSepWith(r.selectors, ppSelector))
            <&> hang(4, text("FROM")    <&> vCommaSepWith(ts, ppTableAlias))
            <&> hang(4, text("WHERE")   <&> ppSQLExpr(noPrec(), r.whereExpr))        
    }

    def vCommaSepWith(xs: List[x], pp: x -> Doc): Doc = 
        foldDocs((x,y) -> x <> char(',') <!> y, List.map(pp, xs))

    pub def ppTableAlias(r: SQLTableAlias): Doc = text(r.tableName) <<>> text("AS") <<>> text(r.variable)

    pub def ppSelector(x: SQLSelector): Doc = match x {
        case SQLAs(e, l)       => ppSQLExpr(noPrec(), e) <<>> text("AS") <<>> text(l)
        case SQLVarAll(v)      => text(v) <> dot() <> char('*')
    }

    /// See Template Haskell for printing with precedence
    /// Precedence support is only a placeholder so far...
    
    instance Pretty[SQLExpr] { 
        pub def pretty(x: SQLExpr): Doc = ppSQLExpr(noPrec(), x)
    }

    pub def ppSQLExpr(n: Int32, x: SQLExpr): Doc = match x { 
        case SQLCaseWhen(e1, e2, e3)   => 
            maybeParens(n > noPrec(), 
                text("CASE") <<>> text("WHEN") <<>> ppSQLExpr(n, e1) <<>> text("THEN") <<>> ppSQLExpr(n, e2) 
                    <<>> text("ELSE") <<>> ppSQLExpr(n, e3) <<>> text("END")
            )

        case SQLConstant(c)            => ppSQLConstant(n, c)      /// TODO
        case SQLFieldProjection(v, l)  => group(text(v) <> dot() <> text(l))
        case SQLAnd(e1, e2)            => ppSQLExpr(n, e1) <<>> text("AND") <<>> ppSQLExpr(n, e2)
        case SQLNot(e1)                => text("NOT EXISTS") <<>> ppSQLExpr(n, e1)
        case SQLExists(q)              => text("EXISTS") <<>> ppQuery(q)
        case SQLPrim(name, es)         => text(name) <<>> tupled(List.map(ppSQLExpr(n), es))
    }

    instance Pretty[SQLConstant] { 
        pub def pretty(x: SQLConstant): Doc = ppSQLConstant(noPrec(), x)
    }

    pub def ppSQLConstant(n: Int32, x: SQLConstant): Doc = match x {
        case SQLBool(b)    => if (b) int32(1) else int32(0)
        case SQLString(s)  => squotes(text(s))
        case SQLInt(i)     => maybeParens((n > noPrec() and (i < 0)), int32(i))
        case SQLNull       => text("null")
    }

    def noPrec(): Int32 = 0

}
