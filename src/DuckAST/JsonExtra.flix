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
    use InteropJson.ToJsonValue

    
    use InteropJson.JsonValue
    use InteropJson.JsonArray
    use InteropJson.JsonObject
    use InteropJson.BuildJsonArray.BuildJsonArray
    use InteropJson.BuildJsonObject.BuildJsonObject

    import jakarta.json.{JsonValue => JJsonValue}
    import jakarta.json.{JsonArray => JJsonArray}
    import jakarta.json.{JsonObject => JJsonObject}
    import java.util.{List => JList}
    import java.util.{Set => JSet}
    import java.util.{Iterator => JIterator}
    import java.util.{Collection => JCollection}
    import java.util.{Map$Entry => JMapEntry}


    pub def addAll(value: t): Unit \ BuildJsonObject with ToJsonValue[t] = 
        match InteropJson.JsonValue.getJsonObject(ToJsonValue.toJsonValue(value)) {
            case Some(o) => BuildJsonObject.addAll(o)
            case None => ()
        }
        

    // On failure generates a Json object with a single field "error" : msg
    pub def evalToJsonObject(o: Option[JsonObject], f: Unit -> a \ ef): JsonObject \ (ef - BuildJsonObject) =
        match InteropJson.BuildJsonObject.evalBuildJsonObject(o, f) {
            case Ok(v)    => v
            case Err(msg) => InteropJson.JsonObject.singleton("error", msg)
        }


    // pub trait ToJsonObject[t] {
    //     pub def toJsonObject(x: t): JsonObject
    // }

    // pub trait ToJsonArray[t] {
    //     pub def toJsonArray(x: t): JsonArray
    // }

    // instance ToJsonObject[JsonObject] {
    //     pub def toJsonObject(x: JsonObject): JsonObject = x
    // }

    // instance ToJsonArray[JsonArray] {
    //     pub def toJsonArray(x: JsonArray): JsonArray = x
    // }

}

