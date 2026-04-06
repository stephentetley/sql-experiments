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
    use InteropJson.JsonArrayBuilder.BuildJsonArray
    use InteropJson.JsonObjectBuilder.BuildJsonObject

    import jakarta.json.{JsonValue => JJsonValue}
    import jakarta.json.{JsonArray => JJsonArray}
    import java.util.{List => JList}
    import java.util.{Iterator => JIterator}

    pub def evalToJsonValue(rc: Region[r], f: Unit -> a \ ef): JsonValue \ (ef - BuildJsonObject + r) =
        match InteropJson.JsonObjectBuilder.evalBuildJsonObjectValue(rc, f) {
            case Err(msg) => jsonObjectSingleton("error", InteropJson.JsonValue.jsonString(msg))
            case Ok(v)    => v
        }


    pub def getJsonArrayElements(x: JsonValue): Option[List[JsonValue]] = region rc {
        if (InteropJson.JsonValue.isArray(x)) {
            let JsonValue.JsonValue(x1) = x;
            let proxy: Proxy[JJsonValue] = Proxy.Proxy; 
            let javaJsonArray: JJsonArray = unchecked_cast(x1 as JJsonArray);
            let javaIter: JIterator[JJsonValue] = unsafe IO { javaJsonArray.iterator() };
            let flixIter = Adaptor.fromIterator(rc, proxy, javaIter);
            let ans = Iterator.toList(flixIter);
            let ans1 = List.map(JsonValue.JsonValue, ans);
            Some(ans1)
        } else None
    }

    pub def jsonArrayFromList(t: List[JsonValue]): JsonValue = region rc {
        let ans = run {
            List.forEach(v -> BuildJsonArray.addJsonValue(v), t)
        } with InteropJson.JsonArrayBuilder.evalBuildJsonArrayValue(rc);
        match ans {
            // Never fails because containter `t` is valid
            case Err(_)   => bug!("unreachable - jsonArrayFromFoldable")
            case Ok(ans1) => ans1
        }
    }

    pub def addToO(name: {name = String}, value: t): Unit \ BuildJsonObject with ToJsonValue[t] = 
        BuildJsonObject.addJsonValue(name, ToJsonValue.toJsonValue(value))

    pub def addToAAt(index: {index = Int32}, value: t): Unit \ BuildJsonArray with ToJsonValue[t] = 
        BuildJsonArray.addJsonValueAt(index, ToJsonValue.toJsonValue(value))


    pub def jsonArrayEmpty(): JsonValue = 
        let value: JJsonValue = checked_cast(unsafe IO { JJsonValue.EMPTY_JSON_ARRAY });
        JsonValue.JsonValue(value)
        
    pub def jsonObjectEmpty(): JsonValue = 
        let value: JJsonValue = checked_cast(unsafe IO { JJsonValue.EMPTY_JSON_OBJECT });
        JsonValue.JsonValue(value)

    pub def jsonObjectFromMap(m: Map[String, JsonValue]): JsonValue = region rc {
        let ans = run {
            Map.forEach((k,v) -> BuildJsonObject.addJsonValue(name = k, v), m)
        } with InteropJson.JsonObjectBuilder.evalBuildJsonObjectValue(rc);
        match ans {
            // Never fails because Map `m` is valid
            case Err(_)   => bug!("unreachable - jsonObjectFromMap")
            case Ok(ans1) => ans1
        }
    }

    pub def jsonArraySingleton(v: JsonValue): JsonValue = region rc {
        let ans = run {
            BuildJsonArray.addJsonValue(v)
        } with InteropJson.JsonArrayBuilder.evalBuildJsonArrayValue(rc);
        match ans {
            // Never fails because `v` are always valid
            case Err(_)   => bug!("unreachable - jsonArraySingleton")
            case Ok(ans1) => ans1
        }
    }

    pub def jsonObjectSingleton(k: String, v: JsonValue): JsonValue = region rc {
        let ans = run {
            BuildJsonObject.addJsonValue(name = k, v)
        } with InteropJson.JsonObjectBuilder.evalBuildJsonObjectValue(rc);
        match ans {
            // Never fails because `k` and `v` are always valid
            case Err(_)   => bug!("unreachable - jsonObjectSingleton")
            case Ok(ans1) => ans1
        }
    }
    

}

