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



namespace Cooper1/ToSql {


    use Cooper1/SourceLang.SrcTerm;
    use Cooper1/SourceLang.SrcTerm.{SrcFor, SrcIfThenElse,
        SrcTable, SrcBagSingleton, SrcBagEmpty, SrcBagUnion, 
        SrcRecord, SrcProj, SrcApp, SrcLam, 
        SrcVar, SrcCst, SrcPrim, SrcEmpty, 
        SrcQuery};
    use Cooper1/SourceLang.SrcConstant;
    use Cooper1/SourceLang.SrcConstant.{SrcBool, SrcString, SrcInt};
    use Cooper1/SourceLang.SrcType;
    use Cooper1/SourceLang.SrcType.{TyRecord, TyBag,
        TyFun, TyBool, TyInt, TyString};

    use Cooper1/SqlLang.SQLQuery;
    use Cooper1/SqlLang.SQLQuery.{SQLUnionAll, SQLSelect};
    use Cooper1/SqlLang.SQLSelectFromWhere.{SQLSelectFromWhere};
    use Cooper1/SqlLang.SQLTableAlias;
    use Cooper1/SqlLang.SQLTableAlias.{SQLTableAs};
    use Cooper1/SqlLang.SQLSelector;
    use Cooper1/SqlLang.SQLSelector.{SQLFieldAs, SQLVarAll};
    use Cooper1/SqlLang.SQLExpr;
    use Cooper1/SqlLang.SQLExpr.{SQLCaseWhen, SQLCst, 
        SQLProj, SQLAnd, SQLNot, SQLExists, 
        SQLPrim};
    use Cooper1/SqlLang.SQLConstant;
    use Cooper1/SqlLang.SQLConstant.{SQLBool, SQLString, SQLInt, SQLNull};
    
  


    pub def translateQNF(t1: SrcTerm): SQLQuery = match t1 {
        case SrcBagUnion(v, u)                      => {
            let v1 = translateQNF(v);
            let u1 = translateQNF(u);
            SQLUnionAll(v1, u1)
        }

        case SrcBagEmpty(TyBag(TyRecord(tys)))      => {
            let r1 = List.map(xy -> SQLFieldAs(SQLCst(SQLNull), fst(xy)), tys);
            SQLSelect(SQLSelectFromWhere(r1, Nil, SQLCst(SQLBool(false))))
        }

        case cnf                                    => translateCNF(cnf)
    }

    pub def translateCNF(t1: SrcTerm): SQLQuery = match t1 { 
        case SrcFor(x, SrcTable(s, _), f)           => {
            let f1 = translateCNF(f);
            let frec = deconsSelect(f1);
            SQLSelect(SQLSelectFromWhere(frec.selectors, SQLTableAs(s, x) :: frec.tables, frec.whereExpr))
        }
        case cb                                     => translateCompBody(cb)
    }


    /// This does the work of the meta-language `where` in Coopers translation.
    def deconsSelect(x: SQLQuery): {selectors :: List[SQLSelector], tables :: List[SQLTableAlias], whereExpr :: SQLExpr} = match x {
        case SQLSelect(SQLSelectFromWhere(ss, ts, e))   => {selectors = ss, tables = ts, whereExpr = e}
        case SQLUnionAll(_, _)                          => bug!("deconsSelect - src not correctly normalized")
        
    }


    pub def translateCompBody(t1: SrcTerm): SQLQuery = match t1 {
        case SrcIfThenElse(b, z, SrcBagEmpty(_))    => {
            let b1 = translateBasicExpr(b);
            let f1 = translateCompBody(z);
            let frec = deconsSelect(f1);
            SQLSelect(SQLSelectFromWhere(frec.selectors, frec.tables, SQLAnd(frec.whereExpr, b1)))
        }

        case SrcBagSingleton(r)                     => {
            let r1 = translateRowForm(r);
            SQLSelect(SQLSelectFromWhere(r1, Nil, SQLCst(SQLBool(true))))

        }

        case SrcTable(s, TyBag(TyRecord(tys)))      => { 
            let xs = List.map(xy -> {let l = fst(xy); SQLFieldAs(SQLProj(s, l), l)}, tys);
            SQLSelect(SQLSelectFromWhere(xs, SQLTableAs(s, s) :: Nil, SQLCst(SQLBool(true))))
        }

        case _                                      => bug!("translateCompBody - src not correctly normalized")
    }


    pub def translateRowForm(s1: SrcTerm): List[SQLSelector] = match s1 {
        case SrcRecord(fs)                          => List.map(translateRecordField, fs)
        case SrcVar(v)                              => SQLVarAll(v) :: Nil
        case _                                      => bug!("translateRowForm - src not correctly normalized")
    }


    def translateRecordField(p1: (String, SrcTerm)): SQLSelector =
        let (l, b) = p1;
        let e1 = translateBasicExpr(b);
        SQLFieldAs(e1, l)

    pub def translateBasicExpr(t1: SrcTerm): SQLExpr = match t1 {
        case SrcIfThenElse(e1, e2, e3)              => {
            let se1 = translateBasicExpr(e1);
            let se2 = translateBasicExpr(e2);
            let se3 = translateBasicExpr(e3);
            SQLCaseWhen(se1, se2, se3)
        }
        
        case SrcEmpty(q)                            => {
            let q1 = translateQNF(q);
            SQLNot(SQLExists(q1))
        }

        case SrcPrim(name, xs)                      => {
            let es = List.map(translateBasicExpr, xs);
            SQLPrim(name, es)
        }

        case SrcProj(SrcVar(v), l)                  => SQLProj(v, l)

        case SrcCst(c)                              => SQLCst(translateConstant(c))

        case _                                      => bug!("translateBasicExpr - src not correctly normalized")
    }


    pub def translateConstant(c1: SrcConstant): SQLConstant = match c1 {
        case SrcBool(b)     => SQLBool(b)
        case SrcString(s)   => SQLString(s)
        case SrcInt(i)      => SQLInt(i)
    }

}
