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



namespace Cooper1/SqlLang {

    use Text/PrettyPrint.Doc;
    use Text/PrettyPrint.{<<>>, <>, <!>, <&>, hang, group, foldDocs, 
        tupled, text, char, int32, squotes, dot};

    pub type alias TableName = String       /// t
    pub type alias FieldName = String       /// l
    pub type alias Variable = String        /// x

    pub enum SQLQuery {
        case SQLUnionAll(SQLQuery, SQLQuery)
        case SQLSelect(SQLSelectFromWhere)
    }


    pub enum SQLSelectFromWhere {
        case SQLSelectFromWhere(List[SQLSelector], List[SQLTableAlias], SQLExpr)
    }

    /// TODO - I think we need a non-alias case...
    pub enum SQLTableAlias {
        case SQLTableAs(TableName, Variable)
    }

    pub enum SQLSelector {
        case SQLFieldAs(SQLExpr, FieldName)
        case SQLVarAll(Variable)
    }

    pub enum SQLExpr {
        case SQLCaseWhen(SQLExpr, SQLExpr, SQLExpr)
        case SQLCst(SQLConstant)
        case SQLProj(Variable, FieldName)
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

    pub def ppSQLQuery(x: SQLQuery): Doc = match x {
        case SQLUnionAll(q1, q2)   => ppSQLQuery(q1) <&> text("UNION ALL") <&> ppSQLQuery(q2)
        case SQLSelect(s)          => ppSQLSelectFromWhere(s)
    }

    /// Empty list of table aliases is a special case
    pub def ppSQLSelectFromWhere(x: SQLSelectFromWhere): Doc = match x {
        case SQLSelectFromWhere(ss, Nil, e)    => 
                hang(4, text("SELECT")  <&> vCommaSep(ss, ppSQLSelector))
            <&> hang(4, text("WHERE")   <&> ppSQLExpr(e))

        case SQLSelectFromWhere(ss, ts, e)     => 
                hang(4, text("SELECT")  <&> vCommaSep(ss, ppSQLSelector))
            <&> hang(4, text("FROM")    <&> vCommaSep(ts, ppSQLTableAlias))
            <&> hang(4, text("WHERE")   <&> ppSQLExpr(e))        
    }

    def vCommaSep(xs: List[x], pp: x -> Doc): Doc = 
        foldDocs((x,y) -> x <> char(',') <!> y, List.map(pp, xs))

    pub def ppSQLTableAlias(x: SQLTableAlias): Doc = match x {
        case SQLTableAs(t, v)           => text(t) <<>> text("AS") <<>> text(v)
    }

    pub def ppSQLSelector(x: SQLSelector): Doc = match x {
        case SQLFieldAs(e, l)           => ppSQLExpr(e) <<>> text("AS") <<>> text(l)
        case SQLVarAll(v)               => text(v) <> dot() <> char('*')
    }

    pub def ppSQLExpr(x: SQLExpr): Doc = match x { 
        case SQLCaseWhen(e1, e2, e3)    => 
            text("CASE") <<>> text("WHEN") <<>> ppSQLExpr(e1) <<>> text("THEN") <<>> ppSQLExpr(e2) 
                <<>> text("ELSE") <<>> ppSQLExpr(e3) <<>> text("END")

        case SQLCst(c)                  => ppSQLConstant(c)
        case SQLProj(v, l)              => group(text(v) <> dot() <> text(l))
        case SQLAnd(e1, e2)             => ppSQLExpr(e1) <<>> text("AND") <<>> ppSQLExpr(e2)
        case SQLNot(e1)                 => text("NOT EXISTS") <<>> ppSQLExpr(e1)
        case SQLExists(q)               => text("EXISTS") <<>> ppSQLQuery(q)
        case SQLPrim(name, es)          => text(name) <<>> tupled(List.map(ppSQLExpr, es))
    }

    pub def ppSQLConstant(x: SQLConstant): Doc = match x {
        case SQLBool(b)     => if (b) int32(1) else int32(0)
        case SQLString(s)   => squotes(text(s))
        case SQLInt(i)      => int32(i)
        case SQLNull        => text("null")
    }

}
