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

    pub def toJsonValueFoldable(t: t[a]): JsonValue \ Foldable.Aef[t] with Foldable[t], ToJsonValue[a] =
        Foldable.toList(t) |> List.map(ToJsonValue.toJsonValue) |> InteropJson.JsonValue.jsonArrayFromList


    pub def evalToJsonValueO(f: Unit -> a \ ef): JsonValue \ (ef - BuildJsonObject) =
        match InteropJson.BuildJsonObject.evalBuildJsonValue(f) {
            case Ok(v)    => v
            case Err(msg) => InteropJson.JsonValue.jsonObjectSingleton("error", InteropJson.JsonValue.jsonString(msg))
        }

    pub def evalToJsonValueA(f: Unit -> a \ ef): JsonValue \ (ef - BuildJsonArray) =
        match InteropJson.BuildJsonArray.evalBuildJsonValue(f) {
            case Ok(v)    => v
            case Err(msg) => InteropJson.JsonValue.jsonObjectSingleton("error", InteropJson.JsonValue.jsonString(msg))
        }

    pub def getJsonObjectNames(x: JsonValue): Option[List[String]] = region rc {
        if (InteropJson.JsonValue.isObject(x)) {
            let JsonValue.JsonValue(x1) = x;
            let javaJsonObject: JJsonObject = unchecked_cast(x1 as JJsonObject);
            let javaKeys: JSet[String] = unsafe IO { javaJsonObject.keySet() };
            let flixIter = Adaptor.fromSetToIterator(rc, javaKeys);
            let ans = Iterator.toList(flixIter);
            Some(ans)
        } else None
    }
    
    pub def getJsonObjectValues(x: JsonValue): Option[List[JsonValue]] = region rc {
        if (InteropJson.JsonValue.isObject(x)) {
            let JsonValue.JsonValue(x1) = x;
            let javaJsonObject: JJsonObject = unchecked_cast(x1 as JJsonObject);
            let javaValues: JCollection[JJsonValue] = unsafe IO { javaJsonObject.values() };
            let flixIter = Adaptor.fromCollectionToIterator(rc, javaValues);
            let ans = Iterator.toList(flixIter);
            let ans1 = List.map(JsonValue.JsonValue, ans);
            Some(ans1)
        } else None
    }    

    def mapEntryKey(x: JMapEntry[k, v]): k = x.getKey()
    def mapEntryValue(x: JMapEntry[k, v]): v = x.getValue()

    pub def getJsonObjectEntries(x: JsonValue): Option[List[(String, JsonValue)]] = region rc {
        if (InteropJson.JsonValue.isObject(x)) {
            let JsonValue.JsonValue(x1) = x;
            let javaJsonObject: JJsonObject = unchecked_cast(x1 as JJsonObject);
            let javaValues: JSet[JMapEntry[String, JJsonValue]] = unsafe IO { javaJsonObject.entrySet() };
            let flixIter = Adaptor.fromSetToIterator(rc, javaValues);
            let ans = Iterator.toList(flixIter);
            let ans1 = List.map(mx -> (mapEntryKey(mx), JsonValue.JsonValue(mapEntryValue(mx))), ans);
            Some(ans1)
        } else None
    }   

}

