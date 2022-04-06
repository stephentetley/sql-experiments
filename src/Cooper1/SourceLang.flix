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


namespace Cooper1/SourceLang {

    pub type alias TableName = String       /// t
    pub type alias FieldName = String       /// l
    pub type alias Variable = String        /// x

    pub enum SrcTerm {
        case SrcFor(Variable, SrcTerm, SrcTerm)
        case SrcIfte(SrcTerm, SrcTerm, SrcTerm)
        case SrcTable(TableName, SrcType)
        case SrcBagSingleton(SrcTerm)
        case SrcBagEmpty(SrcType)
        case SrcBagUnion(SrcTerm, SrcTerm)
        case SrcRecord(List[(FieldName, SrcTerm)])
        case SrcProj(SrcTerm, FieldName)
        case SrcApp(SrcTerm, SrcTerm)
        case SrcLam(Variable, SrcTerm)
        case SrcVar(Variable)
        case SrcCst(SrcConstant)
        case SrcPrim(String, List[SrcTerm])
        case SrcIsEmpty(SrcTerm)
        case SrcQuery(SrcTerm)
    }



    pub enum SrcConstant {
        case SrcBool(Bool)
        case SrcString(String)
        case SrcInt(Int32)
    }

    pub enum SrcType {
        case TyRecord(List[(FieldName, SrcType)])
        case TyBag(SrcType)
        case TyFun(SrcType, SrcType)
        case TyBool
        case TyInt
        case TyString
    }

}


