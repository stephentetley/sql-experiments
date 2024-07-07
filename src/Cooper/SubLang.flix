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


mod Cooper.SubLang {

    use Cooper.Base.{Type, RecordType, Constant}
    use Cooper.Base.FieldName
    use Cooper.Base.TableName
    use Cooper.Base.Variable

    /// Fig 4, page 10

    
    pub enum QueryNF with Eq, ToString {
        case BagUnion(QueryNF, QueryNF)
        case EmptyBag(RecordType)
        case Comprehension(CompNF)
    }

    pub enum CompNF with Eq, ToString {
        case For(Variable, TableName, Type, CompNF)
        case Body(CompBody)
    }

    pub enum CompBody with Eq, ToString {
        case IfThenElseEmpty(BasicExpr, CompBody)
        case BagSingleton(RowForm)
        case Table(TableName, RecordType)
    }


    pub enum RowForm with Eq, ToString {
        case Fields(List[RecordField])
        case RowVar(Variable)
    }

    pub enum RecordField with Eq, ToString { 
        case RecordField(FieldName, BasicExpr)
    }


    pub enum BasicExpr with Eq, ToString {
        case IfThenElse(BasicExpr, BasicExpr, BasicExpr)
        case Empty(QueryNF)
        case Prim(String, List[BasicExpr])
        case FieldProjection(Variable, FieldName)
        case Constant(Constant)
    }


}
