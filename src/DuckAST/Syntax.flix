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

    use InteropJson.JsonValue.JsonValue
    use InteropJson.JsonObjectBuilder.JsonObjectBuilder


    use FlixPretty.Doc
    use FlixPretty.PrettyClass.Pretty
    use FlixPretty.PrettyClass.PrettyLevel
    
    use FlixPretty.{<<>>, <^>, <!>}
    use FlixPretty.{empty, int32, squotes, encloseSep, text, char, comma, dot,
        intersperse, lparen, rparen, parens, tupled, vcat, nest}

    pub enum TODO with Eq, ToString { case TODO }

    pub def toJsonTODO(x: TODO): JsonValue = match x {
        case _          => DuckAST.JsonExtra.createJsonStringValue("TODO")
    }

    pub enum QueryNode with Eq, ToString {
        case CTENode
        case RecursiveCTENode
        case StatementNode
        case SelectNode(QueryNodeProperties, List[ParsedExpression], TableRef, ParsedExpression, TODO, ParsedExpression, ParsedExpression, TODO, TODO)
        case SetOperationNode
    }

    pub def toJsonQueryNode(x: QueryNode): JsonValue = match x {
        case _          => DuckAST.JsonExtra.createJsonStringValue("TODO")
    }


    pub type alias QueryNodeProperties = (QueryNodeType, List[ResultModifier], TODO)

    // Does evalJsonObjectBuilder need to return a Result?
    pub def toJsonQueryNodeProperties(x: QueryNodeProperties): Result[String, JsonValue] = region rc {
        match x {
            case (ty, _, _)          =>
                let ans = run {
                    JsonObjectBuilder.addJsonValue(name = "type", toJsonQueryNodeType(ty));
                    JsonObjectBuilder.addJsonValue(name = "modifiers", DuckAST.JsonExtra.createJsonStringValue("TODO"));
                    JsonObjectBuilder.addJsonValue(name = "???", DuckAST.JsonExtra.createJsonStringValue("TODO"));
                    ()
                } with InteropJson.JsonObjectBuilder.evalJsonObjectBuilder(rc);
                Result.map(DuckAST.JsonExtra.jsonObjectToJsonValue, ans)
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

    pub def toJsonQueryNodeType(x: QueryNodeType): JsonValue = match x {
        case QueryNodeType.SELECT_NODE          => DuckAST.JsonExtra.createJsonStringValue("SELECT_NODE")
        case QueryNodeType.SET_OPERATION_NODE   => DuckAST.JsonExtra.createJsonStringValue("SET_OPERATION_NODE")
        case QueryNodeType.BOUND_SUBQUERY_NODE  => DuckAST.JsonExtra.createJsonStringValue("BOUND_SUBQUERY_NODE")
        case QueryNodeType.RECURSIVE_CTE_NODE   => DuckAST.JsonExtra.createJsonStringValue("RECURSIVE_CTE_NODE")
        case QueryNodeType.CTE_NODE             => DuckAST.JsonExtra.createJsonStringValue("CTE_NODE")
        case QueryNodeType.STATEMENT_NODE       => DuckAST.JsonExtra.createJsonStringValue("STATEMENT_NODE")
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
        case SelectStatement 
        case SetStatement 
        case TransactionStatement 
        case UpdateExtensionsStatement 
        case UpdateStatement 
        case VacuumStatement 
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
    
}

