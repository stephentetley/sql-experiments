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
        SQLStatement, QueryNode, QueryNodeProperties, QueryNodeType, TableRef,
        GroupByNode, AggregateHandling, CommonTableExpressionMap,
        ParsedExpression, Value, Type, Val,
        TableRefProperties, TableReferenceType, 
        ExternalDependency}


    pub def ast01(): JsonResult = 
        let stmt1 = 
            SQLStatement.SelectStatement(
                Map.empty(),
                QueryNode.SelectNode(
                    QueryNodeProperties.QueryNodeProperties(
                        QueryNodeType.SELECT_NODE, 
                        Nil,
                        Map.empty()), 
                    List#{ParsedExpression.ConstantExpression(
                                        Value.Value(Type.Type, 
                                                    false, 
                                                    Val.VInteger(2), 
                                                    None)
                                        ) // Constant
                                    }, 
                    TableRef.EmptyTableRef(
                        TableRefProperties.TableRefProperties(
                            TableReferenceType.EMPTY_FROM,
                            "",
                            None,
                            Some(18446744073709552000ii),
                            ExternalDependency.ExternalDependency,
                            Nil)
                    ), 
                    None,      // where
                    GroupByNode.GroupByNode(Nil, Nil), 
                    None, 
                    None, 
                    AggregateHandling.STANDARD_HANDLING, 
                    None));
        JsonResult.JsonOk(List#{stmt1})

//    List#{ParsedExpression.ConstantExpression(
//                             Value.Value(Type.Type, 
//                                         false, 
//                                         Val.VInteger(2), 
//                                         None)
//                             )
//                         }, // Constant

}

