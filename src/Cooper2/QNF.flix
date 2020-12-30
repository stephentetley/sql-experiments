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


namespace Cooper2/QNF {

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