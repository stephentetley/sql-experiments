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


pub mod DuckAST.JsonExtra { 

    
    use InteropJson.JsonValue.JsonValue
    use InteropJson.JsonValue.JsonArray
    use InteropJson.JsonValue.JsonObject
    use InteropJson.JsonArrayBuilder.BuildJsonArray
    use InteropJson.JsonObjectBuilder.BuildJsonObject

    import jakarta.json.Json
    import jakarta.json.{JsonValue => JJsonValue}


    // TODO - should have a ToJson instance for JsonArray
    pub def jsonArrayToJson(x: JsonArray): JsonValue =
        let JsonArray.JsonArray(x1) = x;
        let jsonvalue: JJsonValue = checked_cast(x1);
        JsonValue.JsonValue(jsonvalue)

    // TODO - should have a ToJson instance for JsonObject
    pub def jsonObjectToJson(x: JsonObject): JsonValue =
        let JsonObject.JsonObject(x1) = x;
        let jsonvalue: JJsonValue = checked_cast(x1);
        JsonValue.JsonValue(jsonvalue)

    pub def evalBuildJsonArrayValue(rc: Region[r], f: Unit -> a \ ef): Result[String, JsonValue] \ (ef - BuildJsonArray + r) =
        InteropJson.JsonArrayBuilder.evalBuildJsonArray(rc, f) |> Result.map(jsonArrayToJson)

    pub def evalBuildJsonObjectValue(rc: Region[r], f: Unit -> a \ ef): Result[String, JsonValue] \ (ef - BuildJsonObject + r) =
        InteropJson.JsonObjectBuilder.evalBuildJsonObject(rc, f) |> Result.map(jsonObjectToJson)


}

