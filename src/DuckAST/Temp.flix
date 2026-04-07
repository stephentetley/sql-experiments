/*
 * Copyright 2026 Stephen Tetley
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


pub mod DuckAST.Temp {
    
    use DuckAST.Syntax.{JsonResult, TODO, 
        SQLStatement, QueryNode, QueryNodeType, TableRef,
        AggregateHandling}


    pub def ast01(): JsonResult = 
        let stmt1 = 
            SQLStatement.SelectStatement(
                Map.empty(),
                QueryNode.SelectNode(
                    (QueryNodeType.SELECT_NODE, Nil, TODO.TODO), 
                    Nil, 
                    TableRef.EmptyTableRef, 
                    None,      // where
                    TODO.TODO, 
                    None, 
                    None, 
                    AggregateHandling.STANDARD_HANDLING, 
                    Some(TODO.TODO)));
        JsonResult.JsonOk(List#{stmt1})

    // pub enum Count with Eq, ToString { 
    //     case Count({count = Int32 })
    // }

    // pub enum Candidate with Eq, ToString {
    //     case Candidate(String, Count)
    // } 
}

