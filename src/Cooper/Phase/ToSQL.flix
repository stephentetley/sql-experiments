/*
 * Copyright 2024 Stephen Tetley
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


mod Cooper.Phase.ToSQL {

    use Cooper.Base.{Type, RecordType, Constant}
    use Cooper.Base.{FieldName}
    use Cooper.SubLang.{QueryNF, CompNF, CompBody, RowForm, RecordField, BasicExpr}
    use Cooper.SQLFragment.{Query, SelectFromWhere, TableAlias, Selector, Expr}

    pub def toSQL(qnf: QueryNF): Query = qnfToSQL(qnf, identity)


    pub def qnfToSQL(x: QueryNF, k: Query -> ans): ans = match x {
        case QueryNF.BagUnion(u, v)                 => qnfToSQL(u, u1 ->
                                                        qnfToSQL(v, v1 ->
                                                            k(Query.UnionAll(u1, v1))))
        case QueryNF.EmptyBag(rt)                   => {
            let ss = getRecordTypeFieldNames(rt) |> List.map(l -> Selector.As(Expr.Null, l));
            k(Query.Select(SelectFromWhere.SelectFromWhere(ss, Nil, Expr.Constant(Constant.Bool(false)))))
        }
        case QueryNF.Comprehension(cnf)             => cnfToSQL(cnf, c1 -> k(Query.Select(c1)))
    }



    def cnfToSQL(x: CompNF, k: SelectFromWhere -> ans): ans = match x {
        case CompNF.For(l, s, _, f)                => 
            cnfToSQL(f, f1 -> match f1 {
                case SelectFromWhere.SelectFromWhere(sels, tabs, e) => 
                    k(SelectFromWhere.SelectFromWhere(sels, TableAlias.TableAlias(s, l) :: tabs, e))
            })
        case CompNF.Body(b)                         => cbToSQL(b, k)
    }

    def cbToSQL(x: CompBody, k: SelectFromWhere -> ans): ans = match x {
        case CompBody.IfThenElseEmpty(b, z)         => 
            beToSQL(b, b1 -> 
                cbToSQL(z, z1 -> match z1 {
                    case SelectFromWhere.SelectFromWhere(sels, tabs, e) => 
                        k(SelectFromWhere.SelectFromWhere(sels, tabs, Expr.And(e, b1)))
                }))
        case CompBody.BagSingleton(r)               => rfToSQL(r, r1 ->
                                                        k(SelectFromWhere.SelectFromWhere(r1, Nil, Expr.Constant(Constant.Bool(true)) )))
        case CompBody.Table(s, rt)                  => {
            let ss = getRecordTypeFieldNames(rt) |> List.map(l -> Selector.As(Expr.FieldProjection(s, l), l));
            k(SelectFromWhere.SelectFromWhere(ss, TableAlias.TableAlias(s, s) :: Nil, Expr.Constant(Constant.Bool(true))))
        }
    }


    def rfToSQL(x: RowForm, k: List[Selector] -> ans): ans = match x {
        case RowForm.Fields(fs)                     => rfListToSQL(fs, fs1 ->
                                                        k(fs1))
        case RowForm.RowVar(r)                      => k(Selector.RowAll(r) :: Nil)
    }

    def rfListToSQL(xs: List[RecordField], k: List[Selector] -> ans): ans = match xs {
        case Nil                                    => k(Nil)
        case RecordField.RecordField(l, b) :: rs    => beToSQL(b, b1 -> 
                                                        rfListToSQL(rs, ks -> 
                                                            k(Selector.As(b1, l) :: ks)))
    }


    def beToSQL(x: BasicExpr, k: Expr -> ans): ans = match x {
        case BasicExpr.IfThenElse(c, t, e)          => beToSQL(c, c1 -> 
                                                        beToSQL(t, t1 -> 
                                                            beToSQL(e, e1 -> 
                                                                k(Expr.CaseWhen(c1, t1, e1)))))
        case BasicExpr.Empty(v)                     => qnfToSQL(v, e1 -> 
                                                        k(Expr.Not(Expr.Exists(e1))))
        case BasicExpr.Prim(s, bs)                  => beListToSQL(bs, ks ->
                                                        k(Expr.Prim(s, ks)))
        case BasicExpr.FieldProjection(s, l)        => k(Expr.FieldProjection(s, l))
        case BasicExpr.Constant(c)                  => k(Expr.Constant(c))
    }

    def beListToSQL(xs: List[BasicExpr], k: List[Expr] -> ans): ans = match xs {
        case Nil            => k(Nil)
        case e :: rs        => beToSQL(e, e1 -> 
                                beListToSQL(rs, ks -> 
                                    k(e1 :: ks)))
    }



    def getRecordTypeFieldNames(rt: RecordType): List[FieldName] = match rt {
        case RecordType.RecordType(xs) => List.map(fst, xs)
    }


}
