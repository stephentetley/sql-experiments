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


mod Cooper.SourceLang {

    use Cooper.Base.{Type, Constant}
    use Cooper.Base.FieldName
    use Cooper.Base.TableName
    use Cooper.Base.Variable

    /// Fig 1, page 5

    pub enum Term with Eq, ToString {
        case For(Variable, Term, Term)
        case IfThenElse(Term, Term, Term)
        case Table(TableName, Type)
        case BagSingleton(Term)
        case BagEmpty
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
    
    pub enum RecordField with Eq, ToString { 
        case RecordField(FieldName, Term)
    }



}
