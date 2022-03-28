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

    pub type alias SLTableName = String       /// t
    pub type alias SLFieldName = String       /// l
    pub type alias SLVariable = String        /// x

    /// Fig 1, page 5

    pub enum SLTerm {
        case SLFor(SLVariable, SLTerm, SLTerm)
        case SLIfThenElse(SLTerm, SLTerm, SLTerm)
        case SLTable(SLTableName, List[SLFieldName])
        case SLBagSingleton(SLTerm)
        case SLBagEmpty
        case SLBagUnion(SLTerm, SLTerm)
        case SLRecord(List[SLRecordField])
        case SLFieldProjection(SLTerm, SLFieldName)
        case SLApply(SLTerm, SLTerm)
        case SLLambda(SLVariable, SLTerm)
        case SLVariable(SLVariable)
        case SLConstant(SLConstant)
        case SLPrim(String, List[SLTerm])
        case SLEmpty(SLTerm)
        case SLQuery(SLTerm)
    }
    
    pub type alias SLRecordField = 
        { fieldName :: SLFieldName
        , term :: SLTerm
        }

    pub enum SLConstant {
        case SLBool(Bool)
        case SLString(String)
        case SLInt(Int32)
    }

}