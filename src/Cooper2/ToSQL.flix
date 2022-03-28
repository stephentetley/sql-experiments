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

    use Cooper2/SourceLanguage.SLTerm;
    use Cooper2/SourceLanguage.SLTerm.{SLFor, SLIfThenElse,
        SLTable, SLBagSingleton, SLBagEmpty, SLBagUnion,
        SLRecord, SLFieldProjection, SLApply, SLLambda,
        SLVariable, SLConstant, SLPrim, SLEmpty, 
        SLQuery};
    use Cooper2/SourceLanguage.SLRecordField;
    use Cooper2/SourceLanguage.SLConstant;
    use Cooper2/SourceLanguage.SLConstant.{SLBool, SLString, SLInt};

    use Cooper2/SQLTarget.{SQLSelectFromWhere, SQLTableAlias};
    use Cooper2/SQLTarget.SQLQuery;
    use Cooper2/SQLTarget.SQLQuery.{SQLUnionAll, SQLSelect};
    use Cooper2/SQLTarget.SQLSelector;
    use Cooper2/SQLTarget.SQLSelector.{SQLAs, SQLVarAll};
    use Cooper2/SQLTarget.SQLExpr;
    use Cooper2/SQLTarget.SQLExpr.{SQLCaseWhen, SQLConstant, SQLFieldProjection, SQLAnd, SQLNot, SQLExists, SQLPrim};
    use Cooper2/SQLTarget.SQLConstant;
    use Cooper2/SQLTarget.SQLConstant.{SQLBool, SQLString, SQLInt, SQLNull};
    
    /// TODO - we need to translate within a monad that handles failure...

  

    pub def translateQueryNormalForm(x: SLTerm): SQLQuery = match x { 
        case SLBagUnion(v, u)  => {
            let v1 = translateQueryNormalForm(v);
            let u1 = translateQueryNormalForm(u);
            SQLUnionAll(v1, u1)
        }

        /// case SLBagEmpty(ls)    => {
        ///     let r1 = List.map(l -> SQLAs(SQLConstant(SQLNull), l), ls);
        ///     SQLSelect({selectors = r1, tableAliases = Nil, whereExpr = SQLConstant(SQLBool(false))})
        /// }

        case z => translateComprehensionNormalForm(z)
    }

    pub def translateComprehensionNormalForm(x: SLTerm) : SQLQuery = match x { 
        case SLFor(v, SLTable(s, _), f)         => {
            let f1 = translateComprehensionNormalForm(f);
            let r = deconsSelect(f1);
            SQLSelect({tableAliases = {tableName = v, variable = s} :: r.tableAliases | r})
        }
        case z                      => translateComprehensionBody(z)
    }

    def deconsSelect(x: SQLQuery): SQLSelectFromWhere = match x {
        case SQLUnionAll(_,_) => bug!("unreachable - x: Query at this point will always be a Select(...)")
        case SQLSelect(r)     => r
    }


    pub def translateComprehensionBody(x: SLTerm): SQLQuery = match x {
        case SLIfThenElse(b, z, SLBagEmpty)    => {
            let b1 = translateBasicExpr(b);
            let f1 = translateComprehensionBody(z);
            let r = deconsSelect(f1);
            SQLSelect({whereExpr = SQLAnd(r.whereExpr, b1) | r})
        }

        case SLBagSingleton(r) => {
            let r1 = translateRowForm(r);
            SQLSelect({selectors = r1, tableAliases = Nil, whereExpr = SQLConstant(SQLBool(true))})
        }

        case SLTable(s, ls)    => { 
            let xs = List.map(l -> SQLAs(SQLFieldProjection(s,l), l), ls);
            SQLSelect({selectors = xs, tableAliases = {tableName = s, variable = s} :: Nil, whereExpr = SQLConstant(SQLBool(true))})
        }

        case _ => bug!("translateComprehensionBody")
    }


    pub def translateRowForm(x: SLTerm): List[SQLSelector] = match x {
        case SLRecord(fs)   => List.map(translateRecordField, fs)
        case SLVariable(v)  => SQLVarAll(v) :: Nil
        case _ => bug!("translateRowForm")
    }

    def translateRecordField(x: SLRecordField): SQLSelector =
        let e1 = translateBasicExpr(x.term);
        SQLAs(e1, x.fieldName)

    pub def translateBasicExpr(x: SLTerm): SQLExpr = match x {
        case SLIfThenElse(e1, e2, e3)  => {
            let se1 = translateBasicExpr(e1);
            let se2 = translateBasicExpr(e2);
            let se3 = translateBasicExpr(e3);
            SQLCaseWhen(se1, se2, se3)
        }
        
        case SLEmpty(q)                => {
            let q1 = translateQueryNormalForm(q);
            SQLNot(SQLExists(q1))
        }

        case SLPrim(name, xs)          => {
            let es = List.map(translateBasicExpr, xs);
            SQLPrim(name, es)
        }

        case SLFieldProjection(SLVariable(v), l)   => SQLFieldProjection(v, l)
        case SLConstant(c)                         => SQLConstant(translateConstant(c))
        case _ => bug!("translateBasicExpr")
    }


    pub def translateConstant(x: SLConstant): SQLConstant = match x {
        case SLBool(b)     => SQLBool(b)
        case SLString(s)   => SQLString(s)
        case SLInt(i)      => SQLInt(i)
    }

}