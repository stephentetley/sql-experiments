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
        intersperse, lparen, rparen, tupled}


    pub enum Relation with Eq, ToString {
        case Select(List[Expression], Option[Relation])
    }

    instance Pretty[Relation] {
        pub override def pPrintPrec(d: PrettyLevel, p: Int32, x: Relation): Doc = 
            def ppSel(es) = text("SELECT") <<>> intersperse(text(", "), List.map(e -> Pretty.pPrintPrec(d, p, e), es));
            match x {
                case Relation.Select(es, None)          => ppSel(es)
                case Relation.Select(es, Some(rel))     => ppSel(es) <<>> text("FROM") <<>> Pretty.pPrintPrec(d, p, rel)
                    
            }
    }


    pub enum Expression with Eq, ToString {
        case Constant(Constant)
        case SetAlias(Expression, String)
        case StarExpression(List[String])
    }

    instance Pretty[Expression] {
        pub override def pPrintPrec(d: PrettyLevel, p: Int32, x: Expression): Doc = match x {
            case Expression.Constant(c)          => Pretty.pPrint(c)
            case Expression.SetAlias(e, name)    => 
                Pretty.pPrintPrec(d, p, e) <<>> text("AS") <<>> text(name)
            case Expression.StarExpression(Nil)  => char('*')
            case Expression.StarExpression(xs)   => 
                char('*') <<>> text("EXCLUDE") <<>> tupled(List.map(text, xs))
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

}