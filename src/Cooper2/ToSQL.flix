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

    use Cooper2/SQLTarget.Query;
    use Cooper2/SQLTarget.Query.{UnionAll, Select};
    use Cooper2/SQLTarget.SelectFromWhere.{SelectFromWhere};
    use Cooper2/SQLTarget.TableAlias;
    use Cooper2/SQLTarget.TableAlias.{TableAlias};
    use Cooper2/SQLTarget.Selector;
    use Cooper2/SQLTarget.Selector.{As, VarAll};
    use Cooper2/SQLTarget.Expr;
    use Cooper2/SQLTarget.Expr.{CaseWhen, Constant, FieldProjection, And, Not, Exists, Prim};
    use Cooper2/SQLTarget.Constant;
    use Cooper2/SQLTarget.Constant.{Bool, String, Int, Null};
    
    pub type alias TableName = String       /// t
    pub type alias FieldName = String       /// l
    pub type alias Variable = String        /// x

    pub def translateQueryNormalForm(x: QueryNormalForm): Query = match x { 
        case QNFBagUnion(v, u)  => {
            let v1 = translateQueryNormalForm(v);
            let u1 = translateQueryNormalForm(u);
            UnionAll(v1, u1)
        }

        case QNFBagEmpty(ls)    => {
            let r1 = List.map(l -> As(Constant(Null), l), ls);
            Select(SelectFromWhere(r1, Nil, Constant(Bool(false))))
        }

        case QNFComprehensionNormalForm(f) => translateComprehensionNormalForm(f)
    }

    pub def translateComprehensionNormalForm(x: QNFComprehensionNormalForm) : Query = match x { 
        case QNFFor(v, s, _, f)         => {
            let f1 = translateComprehensionNormalForm(f);
            let (ss, ts, e) = deconsSelect(f1);
            Select(SelectFromWhere(ss, TableAlias(v, s) :: ts, e))
        }
        case QNFComprehensionBody(z)    => translateComprehensionBody(z)
    }

    def deconsSelect(x: Query): (List[Selector], List[TableAlias], Expr) = match x {
        case UnionAll(_,_) => bug!("unreachable - x: Query at this point will always be a Select(...)")
        case Select(SelectFromWhere(ss, ts, e)) => (ss, ts, e)
    }


    pub def translateComprehensionBody(x: QNFComprehensionBody): Query = match x {
        case QNFIfThen(b, z)    => {
            let b1 = translateBasicExpr(b);
            let f1 = translateComprehensionBody(z);
            let (ss, ts, e) = deconsSelect(f1);
            Select(SelectFromWhere(ss, ts, And(e, b1)))
        }

        case QNFBag(r)          => {
            let r1 = translateRowForm(r);
            Select(SelectFromWhere(r1, Nil, Constant(Bool(true))))

        }

        case QNFTable(s, ls)    => { 
            let xs = List.map(l -> As(FieldProjection(s,l), l), ls);
            Select(SelectFromWhere(xs, TableAlias(s,s) :: Nil, Constant(Bool(true))))
        }
    }


    pub def translateRowForm(x: QNFRowForm): List[Selector] = match x {
        case QNFRecord(fs)   => List.map(translateRecordField, fs)
        case QNFVar(v)       => VarAll(v) :: Nil
    }

    def translateRecordField(x: QNFRecordField): Selector = match x {
        case QNFRecordField(l, b) => 
            let e1 = translateBasicExpr(b);
            As(e1, l)
    }

    pub def translateBasicExpr(x: QNFBasicExpr): Expr = match x {
        case QNFIfThenElse(e1, e2, e3)  => {
            let se1 = translateBasicExpr(e1);
            let se2 = translateBasicExpr(e2);
            let se3 = translateBasicExpr(e3);
            CaseWhen(se1, se2, se3)
        }
        
        case QNFEmpty(q)                => {
            let q1 = translateQueryNormalForm(q);
            Not(Exists(q1))
        }

        case QNFPrim(name, xs)          => {
            let es = List.map(translateBasicExpr, xs);
            Prim(name, es)
        }

        case QNFFieldProjection(v, l)   => FieldProjection(v, l)
        case QNFConstant(c)             => Constant(translateConstant(c))
    }


    def translateConstant(x: QNFConstant): Constant = match x {
        case QNFBool(b)     => Bool(b)
        case QNFString(s)   => String(s)
        case QNFInt(i)      => Int(i)
    }

}