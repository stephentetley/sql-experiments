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


mod IR.SQLIR {

    pub enum Query with Eq, ToString {
        case Select(List[(Term, String)], List[(Table, String)], Condition)
        case SelectStar(List[(Table, String)], Condition)
        case Union(OptAll, Query, Query)
        case Intersect(OptAll, Query, Query)
        case Except(OptAll, Query, Query)
    }

    pub enum OptAll with Eq, ToString { 
        case All
        case NoAll
    }

    // Where does expresssion fit?
    pub enum Condition with Eq, ToString {
        case CondTrue
        case CondFalse
        case Predicate(Predicate)
        case IsNull(OptNot, Term)
        case IsIn(Tuple, OptNot, Query)
        case Exists(Query)
        case CondAnd(Condition, Condition)
        case CondOr(Condition, Condition)
        case CondNot(Condition)
    }

    pub enum OptNot with Eq, ToString { 
        case Not
        case NoNot
    }

    pub type alias Tuple = List[Term]

    pub enum Term with Eq, ToString {
        case CBool(Bool)
        case CString(String)
        case CInt(Int32)
        case Null
    }

    pub enum Table with Eq, ToString {
        case BaseTable(String)
        case Query(Query)
    }

    pub enum Predicate with Eq, ToString {
        case Cast(Term, Type)
        case Like(Term, String)
        case NotLike(Term, String)
        case UnaryOp(UnaryOp, Term)
        case BinOp(Term, BinOp, Term)
    }

    pub enum BinOp with Eq, ToString {
        case Eq
        case Gt
        case Lt
        case GtEq
        case LtEq
        case Concat
    }

    pub enum UnaryOp with Eq, ToString {
        case Negate
    }

    pub enum Type with Eq, ToString {
        case TyBoolean
        case TyVarchar
    }

}
