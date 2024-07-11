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


mod Cooper.Base {

    pub type alias TableName = String       /// t
    pub type alias FieldName = String       /// l
    pub type alias Variable = String        /// x


    pub enum Constant with Eq, ToString {
        case Bool(Bool)
        case String(String)
        case Int(Int32)
    }
    
    pub enum Type with Eq, ToString {
        case BaseType(BaseType)
        case TyRecord(RecordType)
        case TyBag(Type)
        case TyFun(List[Type], Type)    // Has effect...
    }

    pub enum BaseType with Eq, ToString {
        case TyBool
        case TyInt
        case TyString
    }
    
    pub enum RecordType with Eq, ToString {
        case RecordType(List[(FieldName, Type)])
    }



    pub def isRelationTyped(t: Type): Bool = match t {
        case Type.TyBag(Type.TyRecord(_))   => true
        case _                              => false
    }


}
