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


pub mod DuckAST.Syntax {

    use InteropJson.JsonValue
    use InteropJson.JsonObject
    use InteropJson.BuildJsonObject.BuildJsonObject
    use InteropJson.ToJsonValue
    use InteropJson.FromJsonValue

    use FlixPretty.Doc
    use FlixPretty.PrettyClass.Pretty
    use FlixPretty.PrettyClass.PrettyLevel
    
    use FlixPretty.{<<>>, <^>, <!>}
    use FlixPretty.{empty, int32, squotes, encloseSep, text, char, comma, dot,
        intersperse, lparen, rparen, parens, tupled, vcat, nest}


    // idx_t is a large integer but I'm not sure about signedness, make it BigInt 

    pub enum TODO with Eq, ToString { case TODO }

    pub enum JsonResult with Eq, ToString {
        case JsonOk(List[SQLStatement])
        case JsonError(String, String, String, String)
    }

    instance ToJsonValue[JsonResult] {
        pub def toJsonValue(x: JsonResult): JsonValue = match x {
            case JsonResult.JsonOk(statements) => run {
                    InteropJson.BuildJsonObject.add(name = "error", false);
                    InteropJson.BuildJsonObject.add(name = "statements", 
                                                    List.map(ToJsonValue.toJsonValue, statements) |> ToJsonValue.toJsonValue);
                    ()
                } with InteropJson.BuildJsonObject.evalToJsonValue(None)
            case JsonResult.JsonError(etype, msg, subtype, epos) => run {
                    InteropJson.BuildJsonObject.add(name = "error", true);
                    InteropJson.BuildJsonObject.add(name = "error_type", etype);
                    InteropJson.BuildJsonObject.add(name = "error_message", msg);
                    InteropJson.BuildJsonObject.add(name = "error_subtype", subtype);
                    InteropJson.BuildJsonObject.add(name = "position", epos);
                    ()
                } with InteropJson.BuildJsonObject.evalToJsonValue(None)
        } 
    }
    
    


    instance ToJsonValue[TODO] {
        pub def toJsonValue(x: TODO): JsonValue = match x {
            case TODO.TODO => ToJsonValue.toJsonValue("TODO.TODO")
        }
    }

    pub enum Value with Eq, ToString {
        case Value(Type, Bool, Val, Option[ExtraValueInfo])
    }

    instance ToJsonValue[Value] {
        pub def toJsonValue(x: Value): JsonValue = match x {
            case Value.Value(_,_,_,_) => ToJsonValue.toJsonValue("**Value**")
        }
    }

    pub enum Type with Eq, ToString {
        case Type
    }

    instance ToJsonValue[Type] {
        pub def toJsonValue(x: Type): JsonValue = match x {
            case Type.Type => ToJsonValue.toJsonValue("**Type**")
        }
    }

    // value.hpp
    pub enum Val with Eq, ToString {
        case VBool(Bool)
        case VTinyInt(Int32)
        case VInteger(Int32)
    }

    instance ToJsonValue[Val] {
        pub def toJsonValue(x: Val): JsonValue = match x {
            case Val.VBool(b)       => ToJsonValue.toJsonValue(b)
            case Val.VTinyInt(i)    => ToJsonValue.toJsonValue(i)
            case Val.VInteger(i)    => ToJsonValue.toJsonValue(i)
        }
    }

    // value.cpp
    pub enum ExtraValueInfo with Eq, ToString {
        case StringValueInfo
        case NestedValueInfo
    }

    instance ToJsonValue[ExtraValueInfo] {
        pub def toJsonValue(x: ExtraValueInfo): JsonValue = match x {
            case ExtraValueInfo.StringValueInfo => ToJsonValue.toJsonValue("**ExtraValueInfo**")
            case ExtraValueInfo.NestedValueInfo => ToJsonValue.toJsonValue("**ExtraValueInfo**")
        }
    }

    pub enum SQLStatement with Eq, ToString {
        case AlterStatement 
        case AttachStatement 
        case CallStatement 
        case CopyDatabaseStatement 
        case CopyStatement 
        case CreateStatement 
        case DeleteStatement 
        case DetachStatement 
        case DropStatement 
        case ExecuteStatement 
        case ExplainStatement 
        case ExportStatement 
        case ExtensionStatement 
        case InsertStatement 
        case LoadStatement 
        case LogicalPlanStatement 
        case MergeIntoStatement 
        case MultiStatement 
        case PragmaStatement 
        case PrepareStatement 
        case RelationStatement 
        case SelectStatement(NamedParamMap, QueryNode)
        case SetStatement(NamedParamMap)
        case TransactionStatement 
        case UpdateExtensionsStatement 
        case UpdateStatement 
        case VacuumStatement 
    }

    instance ToJsonValue[SQLStatement] {
        pub def toJsonValue(x: SQLStatement): JsonValue = 
            match x {
                case SQLStatement.SelectStatement(npm, node) => run {
                    InteropJson.BuildJsonObject.add(name = "node", node);
                    InteropJson.BuildJsonObject.add(name = "named_param_map", 
                                                    namedParamMapToJsonValue(npm))
                } with InteropJson.BuildJsonObject.evalToJsonValue(None)
                case _          => ToJsonValue.toJsonValue("**SQLStatement**")
            }
    }

    // TODO case insensitive in key...
    pub type alias NamedParamMap = Map[String, Int32]

    // Becomes a JsonArray of key/value objects.
    pub def namedParamMapToJsonValue(x: NamedParamMap): JsonValue = 
        Map.toList(x) 
            |> List.map(match(k, v) -> InteropJson.JsonObject.singleton(k, v))
            |> InteropJson.JsonArray.fromList
            |> InteropJson.JsonArray.toJsonValue


    pub enum QueryNode with Eq, ToString {
        case CTENode
        case RecursiveCTENode
        case StatementNode
        // props * select_list * from_table * where_clause? * groups * having * qualify * aggregate_handling * sample 
        case SelectNode(QueryNodeProperties, List[ParsedExpression], TableRef, Option[ParsedExpression], GroupByNode, Option[ParsedExpression], Option[ParsedExpression], AggregateHandling, Option[SampleOptions])
        case SetOperationNode
    }

    instance ToJsonValue[QueryNode] {
        pub def toJsonValue(x: QueryNode): JsonValue = match x {
            case QueryNode.SelectNode(props, selects, fromt, wherec, groups, havingc, qualifyc, agg, samplec) => 
                run {
                    DuckAST.JsonExtra.addAll(props);
                    InteropJson.BuildJsonObject.add(name = "select_list", selects);
                    InteropJson.BuildJsonObject.add(name = "from_table", fromt);
                    InteropJson.BuildJsonObject.add(name = "where_clause", wherec);
                    InteropJson.BuildJsonObject.add(name = "group_expressions", getGroupExpressions(groups));
                    InteropJson.BuildJsonObject.add(name = "group_sets", getGroupingSets(groups));
                    InteropJson.BuildJsonObject.add(name = "aggregate_handling", agg);
                    InteropJson.BuildJsonObject.add(name = "having", havingc);
                    InteropJson.BuildJsonObject.add(name = "sample", samplec);
                    InteropJson.BuildJsonObject.add(name = "qualify", qualifyc);
                    ()
                } with InteropJson.BuildJsonObject.evalToJsonValue(None)
            case _          => ToJsonValue.toJsonValue("**QueryNode**")
        }
    }

    instance FromJsonValue[QueryNode] {
        pub def fromJsonValue(_x: JsonValue): Option[QueryNode] = None
    }


    pub enum QueryNodeProperties with Eq, ToString {
        case QueryNodeProperties(QueryNodeType, List[ResultModifier], CommonTableExpressionMap)
    }

    instance ToJsonValue[QueryNodeProperties] {
        pub def toJsonValue(x: QueryNodeProperties): JsonValue = 
            match x {
                case QueryNodeProperties.QueryNodeProperties(ty, modifiers, cte_map)   =>
                    run {
                        InteropJson.BuildJsonObject.add(name = "type", ty);
                        InteropJson.BuildJsonObject.add(name = "modifiers", modifiers);
                        let cte_map1 = commonTableExpressionMapToJsonValue(cte_map);
                        InteropJson.BuildJsonObject.add(name = "cte_map", cte_map1);
                        ()
                    } with InteropJson.BuildJsonObject.evalToJsonValue(None)
                    
            }
    }

    pub enum QueryNodeType with Eq, ToString, Order {
        case SELECT_NODE
        case SET_OPERATION_NODE
        case BOUND_SUBQUERY_NODE
        case RECURSIVE_CTE_NODE
        case CTE_NODE
        case STATEMENT_NODE
    }

    instance ToJsonValue[QueryNodeType] {
        pub def toJsonValue(x: QueryNodeType): JsonValue = match x {
            case QueryNodeType.SELECT_NODE          => ToJsonValue.toJsonValue("SELECT_NODE")
            case QueryNodeType.SET_OPERATION_NODE   => ToJsonValue.toJsonValue("SET_OPERATION_NODE")
            case QueryNodeType.BOUND_SUBQUERY_NODE  => ToJsonValue.toJsonValue("BOUND_SUBQUERY_NODE")
            case QueryNodeType.RECURSIVE_CTE_NODE   => ToJsonValue.toJsonValue("RECURSIVE_CTE_NODE")
            case QueryNodeType.CTE_NODE             => ToJsonValue.toJsonValue("CTE_NODE")
            case QueryNodeType.STATEMENT_NODE       => ToJsonValue.toJsonValue("STATEMENT_NODE")
        }
    }

    instance FromJsonValue[QueryNodeType] {
        pub def fromJsonValue(x: JsonValue): Option[QueryNodeType] = 
            match InteropJson.JsonValue.getStringWithDefault("", x) {
                case "SELECT_NODE"            => Some(QueryNodeType.SELECT_NODE)
                case "SET_OPERATION_NODE"     => Some(QueryNodeType.SET_OPERATION_NODE)
                case "BOUND_SUBQUERY_NODE"    => Some(QueryNodeType.BOUND_SUBQUERY_NODE)
                case "RECURSIVE_CTE_NODE"     => Some(QueryNodeType.RECURSIVE_CTE_NODE)
                case "CTE_NODE"               => Some(QueryNodeType.CTE_NODE)
                case "STATEMENT_NODE"         => Some(QueryNodeType.STATEMENT_NODE)
                case _ => None
            }
    }


    pub enum ResultModifier with Eq, ToString {
	    case LimitModifier(Option[ParsedExpression], Option[ParsedExpression])
        case OrderModifier(List[OrderByNode])
        case DistinctModifier(List[ParsedExpression])
        case LimitPercentModifier(Option[ParsedExpression], Option[ParsedExpression])
    }

    instance ToJsonValue[ResultModifier] {
        pub def toJsonValue(x: ResultModifier): JsonValue = match x {
            case ResultModifier.LimitModifier(_, _)     => ToJsonValue.toJsonValue("**ResultModifier**")
            case ResultModifier.OrderModifier(_)        => ToJsonValue.toJsonValue("**ResultModifier**")
            case ResultModifier.DistinctModifier(_)        => ToJsonValue.toJsonValue("**ResultModifier**")
            case ResultModifier.LimitPercentModifier(_, _)     => ToJsonValue.toJsonValue("**ResultModifier**")
        }
    }


    pub enum ResultModifierType with Eq, ToString, Order {
	    case LIMIT_MODIFIER
	    case ORDER_MODIFIER
	    case DISTINCT_MODIFIER
	    case LIMIT_PERCENT_MODIFIER
    }

    instance ToJsonValue[ResultModifierType] {
        pub def toJsonValue(x: ResultModifierType): JsonValue = match x {
            case ResultModifierType.LIMIT_MODIFIER          => ToJsonValue.toJsonValue("LIMIT_MODIFIER")
            case ResultModifierType.ORDER_MODIFIER          => ToJsonValue.toJsonValue("ORDER_MODIFIER")
            case ResultModifierType.DISTINCT_MODIFIER       => ToJsonValue.toJsonValue("DISTINCT_MODIFIER")
            case ResultModifierType.LIMIT_PERCENT_MODIFIER  => ToJsonValue.toJsonValue("LIMIT_PERCENT_MODIFIER")
        }
    }

    instance FromJsonValue[ResultModifierType] {
        pub def fromJsonValue(x: JsonValue): Option[ResultModifierType] = 
            match InteropJson.JsonValue.getStringWithDefault("", x) {
                case "LIMIT_MODIFIER"           => Some(ResultModifierType.LIMIT_MODIFIER)
                case "ORDER_MODIFIER"           => Some(ResultModifierType.ORDER_MODIFIER)
                case "DISTINCT_MODIFIER"        => Some(ResultModifierType.DISTINCT_MODIFIER)
                case "LIMIT_PERCENT_MODIFIER"   => Some(ResultModifierType.LIMIT_PERCENT_MODIFIER)
                case _                          => None
            }
    }

    // query_node.hpp
    pub type alias CommonTableExpressionMap = Map[String, CommonTableExpressionInfo]


    // Becomes a JsonArray of key/value objects.
    pub def commonTableExpressionMapToJsonValue(_x: CommonTableExpressionMap): JsonValue = 
        InteropJson.JsonObject.singleton("map", "**commonTableExpressionMapToJsonValue**")
            |> ToJsonValue.toJsonValue




    // common_table_expression_info.hpp
    pub enum CommonTableExpressionInfo with Eq, ToString {
        case CommonTableExpressionInfo(List[String], List[ParsedExpression], List[ParsedExpression], SQLStatement, CTEMaterialize)
    }

    instance ToJsonValue[CommonTableExpressionInfo] {
        pub def toJsonValue(x: CommonTableExpressionInfo): JsonValue = match x {
            case CommonTableExpressionInfo.CommonTableExpressionInfo(_,_,_,_,_) => 
                    ToJsonValue.toJsonValue("**CommonTableExpressionInfo**")
            
        }
    }



    // cte_materialize.hpp
    pub enum CTEMaterialize with Eq, ToString {
	    case CTE_MATERIALIZE_DEFAULT
	    case CTE_MATERIALIZE_ALWAYS
	    case CTE_MATERIALIZE_NEVER
    }

    instance ToJsonValue[CTEMaterialize] {
        pub def toJsonValue(x: CTEMaterialize): JsonValue = match x {
            case CTEMaterialize.CTE_MATERIALIZE_DEFAULT => ToJsonValue.toJsonValue("CTE_MATERIALIZE_DEFAULT")
            case CTEMaterialize.CTE_MATERIALIZE_ALWAYS  => ToJsonValue.toJsonValue("CTE_MATERIALIZE_ALWAYS")
            case CTEMaterialize.CTE_MATERIALIZE_NEVER   => ToJsonValue.toJsonValue("CTE_MATERIALIZE_NEVER")
        }
    }


    pub enum OrderByNode with Eq, ToString {
        case OrderByNode
    }

    instance ToJsonValue[OrderByNode] {
        pub def toJsonValue(x: OrderByNode): JsonValue = match x {
            case OrderByNode.OrderByNode          => ToJsonValue.toJsonValue("**OrderByNode**")
        }
    }

    pub enum ParsedExpression with Eq, ToString {
        case BetweenExpression 
        case BoundExpression 
        case CaseExpression 
        case CastExpression 
        case CollateExpression 
        case ColumnRefExpression 
        case ComparisonExpression 
        case ConjunctionExpression 
        case ConstantExpression(Value)
        case DefaultExpression 
        case FunctionExpression 
        case LambdaExpression 
        case LambdaRefExpression 
        case OperatorExpression 
        case ParameterExpression 
        case PositionalReferenceExpression 
        case StarExpression 
        case SubqueryExpression 
        case TypeExpression 
        case WindowExpression
    }  

    instance ToJsonValue[ParsedExpression] {
        pub def toJsonValue(x: ParsedExpression): JsonValue = match x {
            case _ => ToJsonValue.toJsonValue("**ParsedExpression**")
        }
    }

    pub enum Constraint with Eq, ToString {
        case CheckConstraint 
        case ForeignKeyConstraint 
        case NotNullConstraint 
        case UniqueConstraint 
    }
    

    pub enum TableRef with Eq, ToString {
        case BaseTableRef 
        case BoundRefWrapper 
        case ColumnDataRef 
        case DelimGetRef 
        case EmptyTableRef(TableRefProperties) 
        case ExpressionListRef 
        case JoinRef 
        case PivotRef 
        case ShowRef 
        case SubqueryRef 
        case TableFunctionRef         
    }

    instance ToJsonValue[TableRef] {
        pub def toJsonValue(x: TableRef): JsonValue = match x {
                case TableRef.EmptyTableRef(props) => ToJsonValue.toJsonValue(props)
                case _        => ToJsonValue.toJsonValue("**TableRef**")
            }
    }

    // tableref.hpp
    pub enum TableRefProperties with Eq, ToString {
        case TableRefProperties(TableReferenceType, String, Option[SampleOptions], Option[BigInt], ExternalDependency, List[String])
    }

    instance ToJsonValue[TableRefProperties] {
        pub def toJsonValue(x: TableRefProperties): JsonValue = match x {
            case TableRefProperties.TableRefProperties(_,_, _, _, _, _) => 
                ToJsonValue.toJsonValue("**TableRefProperties**")
        }
    }


    // tableref_type.hpp
    pub enum TableReferenceType with Eq, ToString {
	    case INVALID
	    case BASE_TABLE
	    case SUBQUERY
	    case JOIN
	    case TABLE_FUNCTION
        case EXPRESSION_LIST
	    case CTE
	    case EMPTY_FROM
	    case PIVOT
	    case SHOW_REF
	    case COLUMN_DATA
	    case DELIM_GET
	    case BOUND_TABLE_REF
    }

    instance ToJsonValue[TableReferenceType] {
        pub def toJsonValue(x: TableReferenceType): JsonValue = match x {
            case TableReferenceType.INVALID         => ToJsonValue.toJsonValue("INVALID")
            case TableReferenceType.BASE_TABLE      => ToJsonValue.toJsonValue("BASE_TABLE")
            case TableReferenceType.SUBQUERY        => ToJsonValue.toJsonValue("SUBQUERY")
            case TableReferenceType.JOIN            => ToJsonValue.toJsonValue("JOIN")
            case TableReferenceType.TABLE_FUNCTION  => ToJsonValue.toJsonValue("TABLE_FUNCTION")
            case TableReferenceType.EXPRESSION_LIST => ToJsonValue.toJsonValue("EXPRESSION_LIST")
            case TableReferenceType.CTE             => ToJsonValue.toJsonValue("CTE")
            case TableReferenceType.EMPTY_FROM      => ToJsonValue.toJsonValue("EMPTY_FROM")
            case TableReferenceType.PIVOT           => ToJsonValue.toJsonValue("PIVOT")
            case TableReferenceType.SHOW_REF        => ToJsonValue.toJsonValue("SHOW_REF")
            case TableReferenceType.COLUMN_DATA     => ToJsonValue.toJsonValue("COLUMN_DATA")
            case TableReferenceType.DELIM_GET       => ToJsonValue.toJsonValue("DELIM_GET")
            case TableReferenceType.BOUND_TABLE_REF => ToJsonValue.toJsonValue("BOUND_TABLE_REF")
        }
    }

    pub enum ExternalDependency with Eq, ToString {
        case ExternalDependency
    }

    instance ToJsonValue[ExternalDependency] {
        pub def toJsonValue(x: ExternalDependency): JsonValue = match x {
            case ExternalDependency.ExternalDependency=> 
                ToJsonValue.toJsonValue("**ExternalDependency**")
        }
    }

    pub enum GroupByNode with Eq, ToString {
        case GroupByNode(List[ParsedExpression], List[GroupingSet])
    }
    
    // instance ToJsonValue[GroupByNode] {
    //     pub def toJsonValue(x: ProjectionIndex): JsonValue = match x {
    //         InteropJson.JsonValue.jsonString("**ProjectionIndex**")
    //     }
    // }

    pub def getGroupExpressions(x: GroupByNode): List[ParsedExpression] = match x {
        case GroupByNode.GroupByNode(xs, _) => xs
    }

    pub def getGroupingSets(x: GroupByNode): List[GroupingSet] = match x {
        case GroupByNode.GroupByNode(_, ys) => ys
    }

    pub type alias GroupingSet = List[ProjectionIndex]
    
    pub enum ProjectionIndex with Eq, ToString {
        case ProjectionIndex
    }

    instance ToJsonValue[ProjectionIndex] {
        pub def toJsonValue(_: ProjectionIndex): JsonValue = InteropJson.JsonValue.jsonString("**ProjectionIndex**")
    }
    


    pub enum ParseInfo with Eq, ToString {
        case AlterInfo 
        case AttachInfo 
        case BoundExportData 
        case CopyDatabaseInfo 
        case CopyInfo 
        case CreateInfo 
        case DetachInfo 
        case DropInfo 
        case LoadInfo 
        case PragmaInfo 
        case ShowSelectInfo 
        case TransactionInfo 
        case UpdateExtensionsInfo 
        case VacuumInfo 
    }


    pub enum AlterInfo with Eq, ToString {
        case AlterDatabaseInfo 
        case AlterScalarFunctionInfo 
        case AlterTableFunctionInfo 
        case AlterTableInfo 
        case AlterViewInfo 
        case ChangeOwnershipInfo 
        case SetColumnCommentInfo 
        case SetCommentInfo 
    }

    pub enum CreateInfo with Eq, ToString {
        case CreateCollationInfo 
        case CreateCoordinateSystemInfo 
        case CreateCopyFunctionInfo 
        case CreateFunctionInfo 
        case CreateIndexInfo 
        case CreateSchemaInfo 
        case CreateSecretInfo 
        case CreateSequenceInfo 
        case CreateTableInfo 
        case CreateTriggerInfo 
        case CreateTypeInfo 
        case CreateViewInfo 
    }

    pub enum AggregateHandling with Eq, ToString, Order {
	    case STANDARD_HANDLING
	    case NO_AGGREGATES_ALLOWED
	    case FORCE_AGGREGATES
    }


    instance ToJsonValue[AggregateHandling] {
        pub def toJsonValue(x: AggregateHandling): JsonValue = match x {
            case AggregateHandling.STANDARD_HANDLING        => ToJsonValue.toJsonValue("STANDARD_HANDLING")
            case AggregateHandling.NO_AGGREGATES_ALLOWED    => ToJsonValue.toJsonValue("NO_AGGREGATES_ALLOWED")
            case AggregateHandling.FORCE_AGGREGATES         => ToJsonValue.toJsonValue("FORCE_AGGREGATES")
        }
    }

    instance FromJsonValue[AggregateHandling] {
        pub def fromJsonValue(x: JsonValue): Option[AggregateHandling] = 
            match InteropJson.JsonValue.getStringWithDefault("", x) {
                case "STANDARD_HANDLING"        => Some(AggregateHandling.STANDARD_HANDLING)
                case "NO_AGGREGATES_ALLOWED"    => Some(AggregateHandling.NO_AGGREGATES_ALLOWED)
                case "FORCE_AGGREGATES"         => Some(AggregateHandling.FORCE_AGGREGATES)
                case _                          => None
            }
    }

    pub enum SampleOptions with Eq, ToString {
         case SampleOptions(Bool, SampleMethod, Int64)
    }

    instance ToJsonValue[SampleOptions] {
        pub def toJsonValue(x: SampleOptions): JsonValue =
            match x {
                case SampleOptions.SampleOptions(is_percent, method, seed)          =>
                    run {
                        InteropJson.BuildJsonObject.add(name = "sample_size", "TODO");
                        InteropJson.BuildJsonObject.add(name = "is_percentage", is_percent);
                        InteropJson.BuildJsonObject.add(name = "method", method);
                        InteropJson.BuildJsonObject.add(name = "seed", seed);
                        ()
                    } with InteropJson.BuildJsonObject.evalToJsonValue(None)
                    
            }
    }

    pub enum SampleMethod with Eq, ToString, Order { 
        case SYSTEM_SAMPLE
        case BERNOULLI_SAMPLE 
        case RESERVOIR_SAMPLE 
        case INVALID
    }

    instance ToJsonValue[SampleMethod] {
        pub def toJsonValue(x: SampleMethod): JsonValue = match x {
            case SampleMethod.SYSTEM_SAMPLE     => ToJsonValue.toJsonValue("SYSTEM_SAMPLE")
            case SampleMethod.BERNOULLI_SAMPLE  => ToJsonValue.toJsonValue("BERNOULLI_SAMPLE")
            case SampleMethod.RESERVOIR_SAMPLE  => ToJsonValue.toJsonValue("RESERVOIR_SAMPLE")
            case SampleMethod.INVALID           => ToJsonValue.toJsonValue("INVALID")
        }
    }

    instance FromJsonValue[SampleMethod] {
        pub def fromJsonValue(x: JsonValue): Option[SampleMethod] = 
            match InteropJson.JsonValue.getStringWithDefault("", x) {
                case "SYSTEM_SAMPLE"    => Some(SampleMethod.SYSTEM_SAMPLE)
                case "BERNOULLI_SAMPLE" => Some(SampleMethod.BERNOULLI_SAMPLE)
                case "RESERVOIR_SAMPLE" => Some(SampleMethod.RESERVOIR_SAMPLE)
                case "INVALID"          => Some(SampleMethod.INVALID)
                case _                  => None
            }
    }


}

