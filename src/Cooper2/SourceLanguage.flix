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


namespace Cooper2/SourceLanguage {

    type alias TableName = String       /// t
    type alias FieldName = String       /// l
    type alias Variable = String        /// x

    pub enum Term {
        case For(Variable, Term, Term)
        case IfThenElse(Term, Term, Term)
        case Table(TableName, List[FieldName])
        case BagSingleton(Term)
        case BagEmpty(List[FieldName])
        case BagUnion(Term, Term)
        case Record(List[RecordField])
        case FieldProjection(Term, FieldName)
        case Apply(Term, Term)
        case Lambda(Variable, Term)
        case Variable(Variable)
        case Constant(Constant)
        case Prim(String, List[Term])
        case Empty(Term)
        case Query(Term)
    }
    
    pub enum RecordField {
        case RecordField(FieldName, Term)
    }

    pub enum Constant {
        case Bool(Bool)
        case String(String)
        case Int(Int32)
    }
}