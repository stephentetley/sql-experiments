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
    use Text.PrettyLevel
    use Text.PrettyPrint.Doc
    use Text.PrettyPrint.{<<>>, empty, int32, squotes, encloseSep, text, char, comma, 
        intersperse, lparen, rparen, parens, tupled}
    

    // TODO add args to read_csv
    pub enum Relation with Eq, ToString {
        case Table(String)
        case Select(List[Expression], Option[Relation])
        case Filter(Relation, Expression)
        case ReadCSV(String)
    }


    def ppFunApp(name: String, args: List[Doc]): Doc = 
        text(name) ++ tupled(args)

    instance Pretty[Relation] {
        pub override def pPrintPrec(d: PrettyLevel, p: Int32, x: Relation): Doc = 
            def ppSel(es) = text("SELECT") <<>> intersperse(text(", "), List.map(e -> Pretty.pPrintPrec(d, p, e), es));
            match x {
                case Relation.Table(name)               => text(name)
                case Relation.Select(es, None)          => ppSel(es)
                case Relation.Select(es, Some(reln))    => 
                    ppSel(es) <<>> text("FROM") <<>> Pretty.pPrintPrec(d, p, reln)
                case Relation.Filter(reln, e)           => 
                    Pretty.pPrintPrec(d, p, reln) <<>> text("WHERE") <<>> parens(Pretty.pPrintPrec(d, p, e))
                case Relation.ReadCSV(path)             => ppFunApp("read_csv", List#{squotes(text(path))})
            }
    }


    pub enum Expression with Eq, ToString {
        case UnaryExpression(UnaryOperator, Expression)
        case BinaryExpression(BinaryOperator, Expression, Expression)
        case Constant(Constant)
        case ColumnExpression(String)
        case ComparisonExpression(ComparisonOperator, Expression, Expression)
        case SetAlias(Expression, String)
        case StarExpression(List[String])
        case FunctionExpression(String, List[Expression])
    }

    instance Pretty[Expression] {
        pub override def pPrintPrec(d: PrettyLevel, p: Int32, x: Expression): Doc = match x {
            case Expression.UnaryExpression(op, e)              => match op {
                case UnaryOperator.Negate           => Pretty.pPrint(op) ++ Pretty.pPrintPrec(d, p, e) 
                case UnaryOperator.IsNull           => Pretty.pPrintPrec(d, p, e) <<>> Pretty.pPrint(op)
                case UnaryOperator.IsNotNull        => Pretty.pPrintPrec(d, p, e) <<>> Pretty.pPrint(op)
            }
            case Expression.BinaryExpression(op, e1, e2)        => 
                Pretty.pPrintPrec(d, p, e1) <<>> Pretty.pPrint(op) <<>> Pretty.pPrintPrec(d, p, e2) 

            case Expression.Constant(c)                         => Pretty.pPrint(c)
            case Expression.ColumnExpression(name)              => text(name)
            case Expression.ComparisonExpression(op, e1, e2)    => 
                Pretty.pPrintPrec(d, p, e1) <<>> Pretty.pPrint(op) <<>> Pretty.pPrintPrec(d, p, e2) 

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

    pub enum ComparisonOperator with Eq, ToString {
        case Equal
        case NotEqual
        case GreaterThan
        case GreaterThanOrEqualTo
        case LessThan
        case LessThanOrEqualTo
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

}

