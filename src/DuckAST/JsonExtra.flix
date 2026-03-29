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

    use InteropJson.JsonObject.JsonObject
    use InteropJson.JsonString.JsonString
    use InteropJson.JsonValue.JsonValue

    import jakarta.json.Json
    import jakarta.json.{JsonValue => JJsonValue}

    pub def createJsonString(s: String): JsonString = 
        JsonString.JsonString(unsafe IO { Json.createValue(s) })

    pub def createJsonStringValue(s: String): JsonValue = 
        let value: JJsonValue = checked_cast(unsafe IO { Json.createValue(s) });
        JsonValue.JsonValue(value)

    // Don't add(?) - shouldn't have created a JsonObject in the first place...
    pub def jsonObjectToJsonValue(o: JsonObject): JsonValue =
        let JsonObject.JsonObject(o1) = o;
        let value: JJsonValue = checked_cast(o1);
        JsonValue.JsonValue(value)

}

