/*
 * Copyright 2025 Stephen Tetley
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


mod IR.DuckIR {

    use Text.Pretty
    use Text.PrettyPrint
    use Text.PrettyLevel
    use Text.PrettyPrint.Doc
    use Text.PrettyPrint.{<<>>, <!>}
    use Text.PrettyPrint.{empty, int32, squotes, encloseSep, text, char, comma, 
        intersperse, lparen, rparen, parens, tupled, vcat, nest}
    
    // DuckDB has precedence levels in the PEG grammar

    def pparen(prec1: Int32, prec2: Int32, x: Doc): Doc = if (prec1 <= prec2) parens(x) else x


    pub enum Type with Eq, ToString {
        case BIGINT
        case BIT
        case BLOB
        case BOOLEAN
        case DATE
        case DOUBLE
        case FLOAT
        case HUGEINT
        case INTEGER
        case INTERVAL
        case SMALLINT
        case SQLNULL
        case TIME_TZ
        case TIME
        case TIMESTAMP_MS
        case TIMESTAMP_NS
        case TIMESTAMP_S
        case TIMESTAMP_TZ
        case TIMESTAMP
        case TINYINT
        case UBIGINT
        case UHUGEINT
        case UINTEGER
        case USMALLINT
        case UTINYINT
        case UUID
        case VARCHAR
        case LIST(Type)
        case STRUCT(List[(String, Type)])
        case MAP(Type, Type)
        case UNION(List[(String, Type)])
    }

    def ppStructFieldPrec(d: PrettyLevel, p: Int32, w: (String, Type)): Doc = 
        let (n, ty) = w;
        text(n) <<>> Pretty.pPrintPrec(d, p, ty)


    instance Pretty[Type] {
        pub override def pPrintPrec(d: PrettyLevel, p: Int32, x: Type): Doc = 
            match x {
                case Type.BIGINT                => text("BIGINT")
                case Type.BIT                   => text("BIT")
                case Type.BLOB                  => text("BLOB")
                case Type.BOOLEAN               => text("BOOLEAN")
                case Type.DATE                  => text("DATE")
                case Type.DOUBLE                => text("DOUBLE")
                case Type.FLOAT                 => text("FLOAT")
                case Type.HUGEINT               => text("HUGEINT")
                case Type.INTEGER               => text("INTEGER")
                case Type.INTERVAL              => text("INTERVAL")
                case Type.SMALLINT              => text("SMALLINT")
                case Type.SQLNULL               => text("SQLNULL")
                case Type.TIME_TZ               => text("TIME_TZ")
                case Type.TIME                  => text("TIME")
                case Type.TIMESTAMP_MS          => text("TIMESTAMP_MS")
                case Type.TIMESTAMP_NS          => text("TIMESTAMP_NS")
                case Type.TIMESTAMP_S           => text("TIMESTAMP_S")
                case Type.TIMESTAMP_TZ          => text("TIMESTAMP_TZ")
                case Type.TIMESTAMP             => text("TIMESTAMP")
                case Type.TINYINT               => text("TINYINT")
                case Type.UBIGINT               => text("UBIGINT")
                case Type.UHUGEINT              => text("UHUGEINT")
                case Type.UINTEGER              => text("UINTEGER")
                case Type.USMALLINT             => text("USMALLINT")
                case Type.UTINYINT              => text("UTINYINT")
                case Type.UUID                  => text("UUID")
                case Type.VARCHAR               => text("VARCHAR")
                case Type.LIST(ty)              =>
                    Pretty.pPrintPrec(d, p, ty) ++ text("[]")
                
                case Type.STRUCT(xs)            =>
                    text("STRUCT") ++ tupled(List.map(t -> ppStructFieldPrec(d, p, t), xs))

                case Type.MAP(tykey, tyval)     =>
                    text("MAP") ++ tupled(List#{Pretty.pPrintPrec(d, p, tykey), Pretty.pPrintPrec(d, p, tyval)})

                case Type.UNION(xs)             =>
                    text("UNION") ++ tupled(List.map(t -> ppStructFieldPrec(d, p, t), xs))
            }
    }

    // TODO add args to read_csv
    pub enum Relation with Eq, ToString {
        case Table(String)
        case Project1(List[Expression])
        case Project(List[Expression], Relation)
        case Filter(Relation, Expression)
        case Limit(Relation, Int64)
        case Offset(Relation, Int64)
        case Cross(Relation, Relation)
        case Join(Relation, Relation, JoinType)     // TODO conditions
        case Sort(Relation, List[Expression])
        case Union(Relation, Relation)
        case Except(Relation, Relation)
        case Intersect(Relation, Relation)
        case ReadCSV(String)
    }


    def ppFunApp(name: String, args: List[Doc]): Doc = 
        text(name) ++ tupled(args)

    instance Pretty[Relation] {
        pub override def pPrintPrec(d: PrettyLevel, p: Int32, x: Relation): Doc = 
            def ppSel(es) = text("SELECT") <<>> intersperse(text(", "), List.map(e -> Pretty.pPrintPrec(d, p, e), es));
            match x {
                case Relation.Table(name)                   => text(name)
                case Relation.Project1(es)                  => ppSel(es)
                case Relation.Project(es, reln)             => 
                    ppSel(es) <<>> text("FROM") <<>> Pretty.pPrintPrec(d, p, reln)
                
                case Relation.Filter(reln, e)               => 
                    Pretty.pPrintPrec(d, p, reln) <<>> text("WHERE") <<>> parens(Pretty.pPrintPrec(d, p, e))
                
                case Relation.Limit(reln, n)                => 
                    Pretty.pPrintPrec(d, p, reln) <<>> text("LIMIT") <<>> PrettyPrint.int64(n)

                case Relation.Offset(reln, offset)          => 
                    Pretty.pPrintPrec(d, p, reln) <<>> text("OFFSET") <<>> PrettyPrint.int64(offset)

                case Relation.Cross(reln1, reln2)           => 
                    Pretty.pPrintPrec(d, p, reln1) <<>> text("CROSS JOIN") <<>> Pretty.pPrintPrec(d, p, reln2) 

                case Relation.Join(reln1, reln2, jtype)     => 
                    Pretty.pPrintPrec(d, p, reln1) <<>> Pretty.pPrint(jtype) <<>> Pretty.pPrintPrec(d, p, reln2) 

                case Relation.Sort(reln, es)     => 
                    Pretty.pPrintPrec(d, p, reln) <<>> text("ORDER BY") 
                        <<>> intersperse(text(", "), List.map(e -> Pretty.pPrintPrec(d, p, e), es)) 

                case Relation.Union(reln1, reln2)           => 
                    Pretty.pPrintPrec(d, p, reln1) <<>> text("UNION") <<>> Pretty.pPrintPrec(d, p, reln2) 

                case Relation.Except(reln1, reln2)          => 
                    Pretty.pPrintPrec(d, p, reln1) <<>> text("EXCEPT") <<>> Pretty.pPrintPrec(d, p, reln2) 

                case Relation.Intersect(reln1, reln2)       => 
                    Pretty.pPrintPrec(d, p, reln1) <<>> text("INTERSECT") <<>> Pretty.pPrintPrec(d, p, reln2) 

                case Relation.ReadCSV(path)                 => ppFunApp("read_csv", List#{squotes(text(path))})
            }
    }

    pub enum JoinType with Eq, ToString {
        case LEFT
        case RIGHT
        case OUTER
        case INNER
        case ANTI
        case SEMI
    }

    instance Pretty[JoinType] {
        pub override def pPrint(x: JoinType): Doc = match x {
            case JoinType.LEFT      => text("LEFT")
            case JoinType.RIGHT     => text("RIGHT")
            case JoinType.OUTER     => text("OUTER")
            case JoinType.INNER     => text("INNER")
            case JoinType.ANTI      => text("ANTI")
            case JoinType.SEMI      => text("SEMI")
        }
    }

    pub enum Expression with Eq, ToString {
        case UnaryExpression(UnaryOperator, Expression)
        case BinaryExpression(BinaryOperator, Expression, Expression)
        case CastExpression(Expression, Type)
        case CaseExpression(List[(Expression, Expression)], Option[Expression])
        case CollateExpression(Expression, String)
        case ConjunctionExpression(ConjunctionOperator, Expression, Expression)
        case Constant(Constant)
        case ColumnExpression(String)
        case ComparisonExpression(ComparisonOperator, Expression, Expression)
        case BetweenExpression(BetweenOperator, Expression, Expression, Expression)
        case SetAlias(Expression, String)
        case StarExpression(List[String])
        case FunctionExpression(String, List[Expression])
    }

    def ppWhenPrec(d: PrettyLevel, p: Int32, w: (Expression, Expression)): Doc = 
        let (e1, e2) = w;
        text("WHEN") <<>> Pretty.pPrintPrec(d, p, e1) <<>> text("THEN") <<>> Pretty.pPrintPrec(d, p, e2)

    def ppElsePrec(d: PrettyLevel, p: Int32, e: Expression): Doc =
        text("ELSE") <<>> Pretty.pPrintPrec(d, p, e) 

    instance Pretty[Expression] {
        pub override def pPrintPrec(d: PrettyLevel, p: Int32, x: Expression): Doc = match x {
            case Expression.UnaryExpression(op, e)              => match op {
                case UnaryOperator.Negate           => Pretty.pPrint(op) ++ Pretty.pPrintPrec(d, p, e) 
                case UnaryOperator.IsNull           => Pretty.pPrintPrec(d, p, e) <<>> Pretty.pPrint(op)
                case UnaryOperator.IsNotNull        => Pretty.pPrintPrec(d, p, e) <<>> Pretty.pPrint(op)
            }
            case Expression.BinaryExpression(op, e1, e2)        => 
                Pretty.pPrintPrec(d, p, e1) <<>> Pretty.pPrint(op) <<>> Pretty.pPrintPrec(d, p, e2) 

            case Expression.CastExpression(e, ty)               => 
                pparen(16, p, 
                    text("CAST") ++ parens(Pretty.pPrintPrec(d, p, e) <<>> text ("AS") <<>> Pretty.pPrintPrec(d, p, ty))
                )

            case Expression.CaseExpression(xs, oe)              => {
                let body = match oe { 
                    case None    => List.map(ppWhenPrec(d, p), xs)
                    case Some(e) => List.map(ppWhenPrec(d, p), xs) ++ List#{ppElsePrec(d, p, e)}
                };
                text("CASE") <!> nest(4, vcat(body)) <!> text("END")
            }
            
            case Expression.CollateExpression(e, name)          => {
                pparen(13, p,
                        Pretty.pPrintPrec(d, p, e) <<>> text("COLLATE") <<>> text(name)
                )
            }

            case Expression.ConjunctionExpression(op, e1, e2)   => {
                let opPrec = conjunctionPrec(op);
                pparen(opPrec, p,
                        Pretty.pPrintPrec(d, p, e1) <<>> Pretty.pPrint(op) <<>> Pretty.pPrintPrec(d, p, e2) 
                )
            }

            case Expression.Constant(c)                         => Pretty.pPrint(c)
            case Expression.ColumnExpression(name)              => text(name)
            case Expression.ComparisonExpression(op, e1, e2)    => {
                let opPrec = comparisonPrec(op);
                pparen(opPrec, p,
                        Pretty.pPrintPrec(d, p, e1) <<>> Pretty.pPrint(op) <<>> Pretty.pPrintPrec(d, p, e2) 
                )
            }
                

            case Expression.BetweenExpression(op, e1, ex, ey)    => 
                pparen(7, p,
                    Pretty.pPrintPrec(d, p, e1) <<>> Pretty.pPrint(op) <<>> Pretty.pPrintPrec(d, p, ex) 
                        <<>> text("AND") <<>> Pretty.pPrintPrec(d, p, ey)
                )

            case Expression.SetAlias(e, name)                   => 
                Pretty.pPrintPrec(d, p, e) <<>> text("AS") <<>> text(name)

            case Expression.StarExpression(Nil)                 => char('*')
            case Expression.StarExpression(xs)                  => 
                char('*') <<>> text("EXCLUDE") <<>> tupled(List.map(text, xs))

            case Expression.FunctionExpression(name, es)      => 
                text(name) ++ tupled(List.map(e -> Pretty.pPrintPrec(d, p, e), es)) 
        }
    }

    pub enum Constant with Eq, ToString  {
        case ConstantNull
        case ConstantVarchar(String)
        case ConstantInteger(Int32)
    }

    instance Pretty[Constant] {
        pub override def pPrint(x: Constant): Doc = match x {
            case Constant.ConstantNull          => text("NULL")
            case Constant.ConstantVarchar(s)    => squotes(text(s))
            case Constant.ConstantInteger(i)    => int32(i)
        }
    }

    // TODO - remove Negate is an operator Function Expression
    pub enum UnaryOperator with Eq, ToString {
        case Negate
        case IsNull
        case IsNotNull
    }

    instance Pretty[UnaryOperator] {
        pub override def pPrint(x: UnaryOperator): Doc = match x {
            case UnaryOperator.Negate       => char('-')
            case UnaryOperator.IsNull       => text("IS NULL")
            case UnaryOperator.IsNotNull    => text("IS NOT NULL")
        }
    }

    // String concat is represented by the `concat` function
    pub enum BinaryOperator with Eq, ToString {
        case Add
        case Subtract
        case Multiply
        case Division
        case FloorDivision
        case Modulo
        case Power
    }

    instance Pretty[BinaryOperator] {
        pub override def pPrint(x: BinaryOperator): Doc = match x {
            case BinaryOperator.Add             => char('+')
            case BinaryOperator.Subtract        => char('-')
            case BinaryOperator.Multiply        => char('*')
            case BinaryOperator.Division        => char('/')
            case BinaryOperator.FloorDivision   => text("//")
            case BinaryOperator.Modulo          => char('%')
            case BinaryOperator.Power           => text("**")
        }
    }

    pub enum ConjunctionOperator with Eq, ToString {
        case Or
        case And
        case Not
    }


    def conjunctionPrec(x: ConjunctionOperator): Int32 = match x {
        case ConjunctionOperator.Or     => 1
        case ConjunctionOperator.And    => 2
        case ConjunctionOperator.Not    => 3
    }

    instance Pretty[ConjunctionOperator] {
        pub override def pPrint(x: ConjunctionOperator): Doc = match x {
            case ConjunctionOperator.Or     => text("OR")
            case ConjunctionOperator.And    => text("AND")
            case ConjunctionOperator.Not    => text("NOT")
        }
    }

    pub enum ComparisonOperator with Eq, ToString {
        case Equal
        case NotEqual
        case GreaterThan
        case GreaterThanOrEqualTo
        case LessThan
        case LessThanOrEqualTo
    }

    def comparisonPrec(x: ComparisonOperator): Int32 = match x {
        case ComparisonOperator.Equal                   => 6
        case ComparisonOperator.NotEqual                => 6
        case ComparisonOperator.GreaterThan             => 6
        case ComparisonOperator.GreaterThanOrEqualTo    => 6
        case ComparisonOperator.LessThan                => 6
        case ComparisonOperator.LessThanOrEqualTo       => 6
    }

    instance Pretty[ComparisonOperator] {
        pub override def pPrint(x: ComparisonOperator): Doc = match x {
            case ComparisonOperator.Equal                   => char('=')
            case ComparisonOperator.NotEqual                => text("<>")
            case ComparisonOperator.GreaterThan             => char('>')
            case ComparisonOperator.GreaterThanOrEqualTo    => text(">=")
            case ComparisonOperator.LessThan                => char('<')
            case ComparisonOperator.LessThanOrEqualTo       => text("<=")
        }
    }

    pub enum BetweenOperator with Eq, ToString {
        case Between
        case NotBetween
    }

    instance Pretty[BetweenOperator] {
        pub override def pPrint(x: BetweenOperator): Doc = match x {
            case BetweenOperator.Between        => text("BETWEEN")
            case BetweenOperator.NotBetween     => text("NOT BETWEEN")
        }
    }

}

