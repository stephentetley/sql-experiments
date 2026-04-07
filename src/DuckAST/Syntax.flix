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
    use InteropJson.BuildJsonObject.BuildJsonObject
    use InteropJson.ToJsonValue
    use InteropJson.FromJsonValue

    use FlixPretty.Doc
    use FlixPretty.PrettyClass.Pretty
    use FlixPretty.PrettyClass.PrettyLevel
    
    use FlixPretty.{<<>>, <^>, <!>}
    use FlixPretty.{empty, int32, squotes, encloseSep, text, char, comma, dot,
        intersperse, lparen, rparen, parens, tupled, vcat, nest}


    pub enum TODO with Eq, ToString { case TODO }

    pub enum JsonResult with Eq, ToString {
        case JsonOk(List[SQLStatement])
        case JsonError(String, String, String, String)
    }

    instance ToJsonValue[JsonResult] {
        pub def toJsonValue(x: JsonResult): JsonValue = match x {
            case JsonResult.JsonOk(statements) => run {
                    InteropJson.BuildJsonObject.add(name = "error", false);
                    BuildJsonObject.addJsonValue(name = "statements", DuckAST.JsonExtra.toJsonValueFoldable(statements));
                    ()
                } with DuckAST.JsonExtra.evalToJsonValueO
            case JsonResult.JsonError(etype, msg, subtype, epos) => run {
                    InteropJson.BuildJsonObject.add(name = "error", true);
                    InteropJson.BuildJsonObject.add(name = "error_type", etype);
                    InteropJson.BuildJsonObject.add(name = "error_message", msg);
                    InteropJson.BuildJsonObject.add(name = "error_subtype", subtype);
                    InteropJson.BuildJsonObject.add(name = "position", epos);
                    ()
                } with DuckAST.JsonExtra.evalToJsonValueO
        } 
    }
    
    


    instance ToJsonValue[TODO] {
        pub def toJsonValue(x: TODO): JsonValue = match x {
            case _          => ToJsonValue.toJsonValue("TODO")
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
                case SQLStatement.SelectStatement(_npm, node) => run {
                    InteropJson.BuildJsonObject.add(name = "node", node);
                    InteropJson.BuildJsonObject.add(name = "named_param_map", "TODO")
                } with DuckAST.JsonExtra.evalToJsonValueO
                case _          => ToJsonValue.toJsonValue("TODO")
            }
    }

    // TODO case insensitive in key...
    pub type alias NamedParamMap = Map[String, TODO]

    pub enum QueryNode with Eq, ToString {
        case CTENode
        case RecursiveCTENode
        case StatementNode
        // props * select_list * from_table * where_clause? * groups * having * qualify * aggregate_handling * sample 
        case SelectNode(QueryNodeProperties, List[ParsedExpression], TableRef, Option[ParsedExpression], TODO, Option[ParsedExpression], Option[ParsedExpression], AggregateHandling, Option[TODO])
        case SetOperationNode
    }

    instance ToJsonValue[QueryNode] {
        pub def toJsonValue(x: QueryNode): JsonValue = match x {
            case QueryNode.SelectNode(_props, _exprs, _fromt, _wherec, _groups, _havingc, _qualifyc, agg, _samplec) => 
                run {
                    InteropJson.BuildJsonObject.add(name = "from_table", "TODO");
                    InteropJson.BuildJsonObject.add(name = "where_clause", "TODO");
                    InteropJson.BuildJsonObject.add(name = "aggregate_handling", agg);
                    InteropJson.BuildJsonObject.add(name = "having", "TODO");
                    InteropJson.BuildJsonObject.add(name = "sample", "TODO");
                    InteropJson.BuildJsonObject.add(name = "qualify", "TODO");
                    ()
                } with DuckAST.JsonExtra.evalToJsonValueO
            case _          => ToJsonValue.toJsonValue("TODO")
        }
    }

    instance FromJsonValue[QueryNode] {
        pub def fromJsonValue(_x: JsonValue): Option[QueryNode] = None
    }

    // TODO record?
    pub type alias QueryNodeProperties = (QueryNodeType, List[ResultModifier], TODO)

    
    pub def toJsonQueryNodeProperties(x: QueryNodeProperties): JsonValue = 
        match x {
            case (ty, _modifiers, _cte_map)          =>
                run {
                    InteropJson.BuildJsonObject.add(name = "type", ty);
                    InteropJson.BuildJsonObject.add(name = "modifiers", "TODO");
                    InteropJson.BuildJsonObject.add(name = "cte_map", "TODO");
                    ()
                } with DuckAST.JsonExtra.evalToJsonValueO
                
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
	    case ResultModifier(ResultModifierType, TODO)
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

    pub enum ParsedExpression with Eq, ToString {
        case BetweenExpression 
        case BoundExpression 
        case CaseExpression 
        case CastExpression 
        case CollateExpression 
        case ColumnRefExpression 
        case ComparisonExpression 
        case ConjunctionExpression 
        case ConstantExpression 
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
        case EmptyTableRef 
        case ExpressionListRef 
        case JoinRef 
        case PivotRef 
        case ShowRef 
        case SubqueryRef 
        case TableFunctionRef         
    }

    pub type alias TableRefPropertiess = (Option[SampleOptions])

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

    pub type alias SampleOptions = (Bool, SampleMethod, Int64)

    pub def toJsonSampleOptions(x: SampleOptions): JsonValue =
        match x {
            case (is_percent, method, seed)          =>
                run {
                    InteropJson.BuildJsonObject.add(name = "sample_size", "TODO");
                    InteropJson.BuildJsonObject.add(name = "is_percentage", is_percent);
                    InteropJson.BuildJsonObject.add(name = "method", method);
                    InteropJson.BuildJsonObject.add(name = "seed", seed);
                    ()
                } with DuckAST.JsonExtra.evalToJsonValueO
                
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

