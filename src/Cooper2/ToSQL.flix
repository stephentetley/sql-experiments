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




namespace Cooper2/ToSQL {

    use Cooper2/QNF.{QueryNormalForm};
    use Cooper2/QNF.QueryNormalForm.{BagUnion => QNFBagUnion, 
        BagEmpty => QNFBagEmpty, ComprehensionNormalForm => QNFComprehensionNormalForm};
    use Cooper2/QNF.{ComprehensionNormalForm => QNFComprehensionNormalForm};
    use Cooper2/QNF.ComprehensionNormalForm.{For => QNFFor, ComprehensionBody => QNFComprehensionBody};
    use Cooper2/QNF.{ComprehensionBody => QNFComprehensionBody};
    use Cooper2/QNF.ComprehensionBody.{IfThen => QNFIfThen, Bag => QNFBag, Table => QNFTable};
    use Cooper2/QNF.{RowForm => QNFRowForm};
    use Cooper2/QNF.RowForm.{Record => QNFRecord, Var => QNFVar};
    use Cooper2/QNF.{RecordField => QNFRecordField};
    use Cooper2/QNF.RecordField.{RecordField => QNFRecordField};
    use Cooper2/QNF.{BasicExpr => QNFBasicExpr};
    use Cooper2/QNF.BasicExpr.{IfThenElse => QNFIfThenElse, Empty => QNFEmpty, 
        Prim => QNFPrim, FieldProjection => QNFFieldProjection, Constant => QNFConstant};
    use Cooper2/QNF.{Constant => QNFConstant};
    use Cooper2/QNF.Constant.{Bool => QNFBool, String => QNFString, Int => QNFInt};

    use Cooper2/SQLTarget.{SQLSelectFromWhere, SQLTableAlias};
    use Cooper2/SQLTarget.SQLQuery;
    use Cooper2/SQLTarget.SQLQuery.{SQLUnionAll, SQLSelect};
    use Cooper2/SQLTarget.SQLSelector;
    use Cooper2/SQLTarget.SQLSelector.{SQLAs, SQLVarAll};
    use Cooper2/SQLTarget.SQLExpr;
    use Cooper2/SQLTarget.SQLExpr.{SQLCaseWhen, SQLConstant, SQLFieldProjection, SQLAnd, SQLNot, SQLExists, SQLPrim};
    use Cooper2/SQLTarget.SQLConstant;
    use Cooper2/SQLTarget.SQLConstant.{SQLBool, SQLString, SQLInt, SQLNull};
    
    pub type alias TableName = String       /// t
    pub type alias FieldName = String       /// l
    pub type alias Variable = String        /// x

    pub def translateQueryNormalForm(x: QueryNormalForm): SQLQuery = match x { 
        case QNFBagUnion(v, u)  => {
            let v1 = translateQueryNormalForm(v);
            let u1 = translateQueryNormalForm(u);
            SQLUnionAll(v1, u1)
        }

        case QNFBagEmpty(ls)    => {
            let r1 = List.map(l -> SQLAs(SQLConstant(SQLNull), l), ls);
            SQLSelect({selectors = r1, tableAliases = Nil, whereExpr = SQLConstant(SQLBool(false))})
        }

        case QNFComprehensionNormalForm(f) => translateComprehensionNormalForm(f)
    }

    pub def translateComprehensionNormalForm(x: QNFComprehensionNormalForm) : SQLQuery = match x { 
        case QNFFor(v, s, _, f)         => {
            let f1 = translateComprehensionNormalForm(f);
            let r = deconsSelect(f1);
            SQLSelect({tableAliases = {tableName = v, variable = s} :: r.tableAliases | r})
        }
        case QNFComprehensionBody(z)    => translateComprehensionBody(z)
    }

    def deconsSelect(x: SQLQuery): SQLSelectFromWhere = match x {
        case SQLUnionAll(_,_) => bug!("unreachable - x: Query at this point will always be a Select(...)")
        case SQLSelect(r)     => r
    }


    pub def translateComprehensionBody(x: QNFComprehensionBody): SQLQuery = match x {
        case QNFIfThen(b, z)    => {
            let b1 = translateBasicExpr(b);
            let f1 = translateComprehensionBody(z);
            let r = deconsSelect(f1);
            SQLSelect({whereExpr = SQLAnd(r.whereExpr, b1) | r})
        }

        case QNFBag(r)          => {
            let r1 = translateRowForm(r);
            SQLSelect({selectors = r1, tableAliases = Nil, whereExpr = SQLConstant(SQLBool(true))})
        }

        case QNFTable(s, ls)    => { 
            let xs = List.map(l -> SQLAs(SQLFieldProjection(s,l), l), ls);
            SQLSelect({selectors = xs, tableAliases = {tableName = s, variable = s} :: Nil, whereExpr = SQLConstant(SQLBool(true))})
        }
    }


    pub def translateRowForm(x: QNFRowForm): List[SQLSelector] = match x {
        case QNFRecord(fs)   => List.map(translateRecordField, fs)
        case QNFVar(v)       => SQLVarAll(v) :: Nil
    }

    def translateRecordField(x: QNFRecordField): SQLSelector = match x {
        case QNFRecordField(l, b) => 
            let e1 = translateBasicExpr(b);
            SQLAs(e1, l)
    }

    pub def translateBasicExpr(x: QNFBasicExpr): SQLExpr = match x {
        case QNFIfThenElse(e1, e2, e3)  => {
            let se1 = translateBasicExpr(e1);
            let se2 = translateBasicExpr(e2);
            let se3 = translateBasicExpr(e3);
            SQLCaseWhen(se1, se2, se3)
        }
        
        case QNFEmpty(q)                => {
            let q1 = translateQueryNormalForm(q);
            SQLNot(SQLExists(q1))
        }

        case QNFPrim(name, xs)          => {
            let es = List.map(translateBasicExpr, xs);
            SQLPrim(name, es)
        }

        case QNFFieldProjection(v, l)   => SQLFieldProjection(v, l)

        case QNFConstant(c)             => SQLConstant(translateConstant(c))
    }


    def translateConstant(x: QNFConstant): SQLConstant = match x {
        case QNFBool(b)     => SQLBool(b)
        case QNFString(s)   => SQLString(s)
        case QNFInt(i)      => SQLInt(i)
    }

}