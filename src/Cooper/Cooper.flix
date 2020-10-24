/// Cooper.flix

namespace Cooper/NormalForm {

    type alias TableName = String       /// t
    type alias FieldName = String       /// l
    type alias Variable = String        /// x

    pub enum QueryNormalForm {
        case BagUnion(QueryNormalForm, QueryNormalForm)
        case BagEmpty(List[FieldName])
        case ComprehensionNormalForm(ComprehensionNormalForm)
    }

    pub enum ComprehensionNormalForm {
        case For(Variable, TableName, List[FieldName], ComprehensionNormalForm)
        case ComprehensionBody(ComprehensionBody)
    }

    pub enum ComprehensionBody {
        case IfThen(BasicExpr, ComprehensionBody)
        case Bag(RowForm)
        case Table(TableName, List[FieldName])
    }

    pub enum RowForm {
        case Record(List[RecordField])
        case Var(Variable)
    }

    pub enum RecordField {
        case RecordField(FieldName, BasicExpr)
    }

    pub enum BasicExpr {
        case IfThenElse(BasicExpr, BasicExpr, BasicExpr)
        case Empty(QueryNormalForm)
        case Prim(String, List[BasicExpr])
        case FieldProjection(Variable, FieldName)
        case Constant(Constant)
    }

    pub enum Constant {
        case Bool(Bool)
        case String(String)
        case Int(Int32)
    }


}


namespace Cooper/SqlFragment {

    use Text/PrettyPrint.Doc;
    use Text/PrettyPrint.{<<>>, <>, <!>, <&>, hang, group, foldDocs, 
        tupled, text, char, int32, squotes, dot};

    type alias TableName = String       /// t
    type alias FieldName = String       /// l
    type alias Variable = String        /// x

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

    pub def ppQuery(x: Query): Doc = match x {
        case UnionAll(q1, q2)   => ppQuery(q1) <&> text("UNION ALL") <&> ppQuery(q2)
        case Select(s)          => ppSelectFromWhere(s)
    }

    /// Empty list of table aliases is a special case
    pub def ppSelectFromWhere(x: SelectFromWhere): Doc = match x {
        case SelectFromWhere(ss, Nil, e)    => 
                hang(4, text("SELECT")  <&> vCommaSep(ss, ppSelector))
            <&> hang(4, text("WHERE")   <&> ppExpr(e))

        case SelectFromWhere(ss, ts, e)     => 
                hang(4, text("SELECT")  <&> vCommaSep(ss, ppSelector))
            <&> hang(4, text("FROM")    <&> vCommaSep(ts, ppTableAlias))
            <&> hang(4, text("WHERE")   <&> ppExpr(e))        
    }

    def vCommaSep(xs: List[x], pp: x -> Doc): Doc = 
        foldDocs((x,y) -> x <> char(',') <!> y, List.map(pp, xs))

    pub def ppTableAlias(x: TableAlias): Doc = match x {
        case TableAlias(t, v) => text(t) <<>> text("AS") <<>> text(v)
    }

    pub def ppSelector(x: Selector): Doc = match x {
        case As(e, l)       => ppExpr(e) <<>> text("AS") <<>> text(l)
        case VarAll(v)      => text(v) <> dot() <> char('*')
    }

    pub def ppExpr(x: Expr): Doc = match x { 
        case CaseWhen(e1, e2, e3)   => 
            text("CASE") <<>> text("WHEN") <<>> ppExpr(e1) <<>> text("THEN") <<>> ppExpr(e2) 
                <<>> text("ELSE") <<>> ppExpr(e3) <<>> text("END")

        case Constant(c)            => ppConstant(c)
        case FieldProjection(v, l)  => group(text(v) <> dot() <> text(l))
        case And(e1, e2)            => ppExpr(e1) <<>> text("AND") <<>> ppExpr(e2)
        case Not(e1)                => text("NOT EXISTS") <<>> ppExpr(e1)
        case Exists(q)              => text("EXISTS") <<>> ppQuery(q)
        case Prim(name, es)         => text(name) <<>> tupled(List.map(ppExpr, es))
    }

    pub def ppConstant(x: Constant): Doc = match x {
        case Bool(b)    => if (b) int32(1) else int32(0)
        case String(s)  => squotes(text(s))
        case Int(i)     => int32(i)
        case Null       => text("null")
    }

}

namespace Cooper/SqlTranslation {

    use Cooper/NormalForm.{QueryNormalForm => NFQueryNormalForm};
    use Cooper/NormalForm.QueryNormalForm.{BagUnion => NFBagUnion, 
        BagEmpty => NFBagEmpty, ComprehensionNormalForm => NFComprehensionNormalForm};
    use Cooper/NormalForm.{ComprehensionNormalForm => NFComprehensionNormalForm};
    use Cooper/NormalForm.ComprehensionNormalForm.{For => NFFor, ComprehensionBody => NFComprehensionBody};
    use Cooper/NormalForm.{ComprehensionBody => NFComprehensionBody};
    use Cooper/NormalForm.ComprehensionBody.{IfThen => NFIfThen, Bag => NFBag, Table => NFTable};
    use Cooper/NormalForm.{RowForm => NFRowForm};
    use Cooper/NormalForm.RowForm.{Record => NFRecord, Var => NFVar};
    use Cooper/NormalForm.{RecordField => NFRecordField};
    use Cooper/NormalForm.RecordField.{RecordField => NFRecordField};
    use Cooper/NormalForm.{BasicExpr => NFBasicExpr};
    use Cooper/NormalForm.BasicExpr.{IfThenElse => NFIfThenElse, Empty => NFEmpty, 
        Prim => NFPrim, FieldProjection => NFFieldProjection, Constant => NFConstant};
    use Cooper/NormalForm.{Constant => NFConstant};
    use Cooper/NormalForm.Constant.{Bool => NFBool, String => NFString, Int => NFInt};

    use Cooper/SqlFragment.Query;
    use Cooper/SqlFragment.Query.{UnionAll, Select};
    use Cooper/SqlFragment.SelectFromWhere.{SelectFromWhere};
    use Cooper/SqlFragment.TableAlias;
    use Cooper/SqlFragment.TableAlias.{TableAlias};
    use Cooper/SqlFragment.Selector;
    use Cooper/SqlFragment.Selector.{As, VarAll};
    use Cooper/SqlFragment.Expr;
    use Cooper/SqlFragment.Expr.{CaseWhen, Constant, FieldProjection, And, Not, Exists, Prim};
    use Cooper/SqlFragment.Constant;
    use Cooper/SqlFragment.Constant.{Bool, String, Int, Null};
    
    type alias TableName = String       /// t
    type alias FieldName = String       /// l
    type alias Variable = String        /// x

    pub def translateQueryNormalForm(x: NFQueryNormalForm): Query = match x { 
        case NFBagUnion(v, u)   => {
            let v1 = translateQueryNormalForm(v);
            let u1 = translateQueryNormalForm(u);
            UnionAll(v1, u1)
        }

        case NFBagEmpty(ls)     => {
            let r1 = List.map(l -> As(Constant(Null), l), ls);
            Select(SelectFromWhere(r1, Nil, Constant(Bool(false))))
        }

        case NFComprehensionNormalForm(f) => translateComprehensionNormalForm(f)
    }

    pub def translateComprehensionNormalForm(x: NFComprehensionNormalForm) : Query = match x { 
        case NFFor(v, s, _, f)         => {
            let f1 = translateComprehensionNormalForm(f);
            let (ss, ts, e) = deconsSelect(f1);
            Select(SelectFromWhere(ss, TableAlias(v, s) :: ts, e))
        }
        case NFComprehensionBody(z)     => translateComprehensionBody(z)
    }

    def deconsSelect(x: Query): (List[Selector], List[TableAlias], Expr) = match x {
        case UnionAll(_,_) => panic("unreachable - x: Query at this point will always be a Select(...)")
        case Select(SelectFromWhere(ss, ts, e)) => (ss, ts, e)
    }


    pub def translateComprehensionBody(x: NFComprehensionBody): Query = match x {
        case NFIfThen(b, z)     => {
            let b1 = translateBasicExpr(b);
            let f1 = translateComprehensionBody(z);
            let (ss, ts, e) = deconsSelect(f1);
            Select(SelectFromWhere(ss, ts, And(e, b1)))
        }

        case NFBag(r)           => {
            let r1 = translateRowForm(r);
            Select(SelectFromWhere(r1, Nil, Constant(Bool(true))))

        }

        case NFTable(s, ls)     => { 
            let xs = List.map(l -> As(FieldProjection(s,l), l), ls);
            Select(SelectFromWhere(xs, TableAlias(s,s) :: Nil, Constant(Bool(true))))
        }
    }


    pub def translateRowForm(x: NFRowForm): List[Selector] = match x {
        case NFRecord(fs)   => List.map(translateRecordField, fs)
        case NFVar(v)       => VarAll(v) :: Nil
    }

    def translateRecordField(x: NFRecordField): Selector = match x {
        case NFRecordField(l, b) => 
            let e1 = translateBasicExpr(b);
            As(e1, l)
    }

    pub def translateBasicExpr(x: NFBasicExpr): Expr = match x {
        case NFIfThenElse(e1, e2, e3)   => {
            let se1 = translateBasicExpr(e1);
            let se2 = translateBasicExpr(e2);
            let se3 = translateBasicExpr(e3);
            CaseWhen(se1, se2, se3)
        }
        
        case NFEmpty(q)                 => {
            let q1 = translateQueryNormalForm(q);
            Not(Exists(q1))
        }

        case NFPrim(name, xs)           => {
            let es = List.map(translateBasicExpr, xs);
            Prim(name, es)
        }

        case NFFieldProjection(v, l)    => FieldProjection(v, l)
        case NFConstant(c)              => Constant(translateConstant(c))
    }


    pub def translateConstant(x: NFConstant): Constant = match x {
        case NFBool(b)      => Bool(b)
        case NFString(s)    => String(s)
        case NFInt(i)       => Int(i)
    }
}