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


pub mod DuckAST.JsonFilter { 
    use DuckAST.JsonFilter.JsonFilter
    
        
    use InteropJson.JsonValue

    pub type alias JsonFilter = JsonValue -> List[JsonValue]

    pub def isObj(): JsonFilter = x -> 
        if (JsonValue.isObject(x)) List#{x} else List#{}
    
    pub def isArr(): JsonFilter = x -> 
        if (JsonValue.isArray(x)) List#{x} else List#{}

    pub def isStr(): JsonFilter = x -> 
        if (JsonValue.isString(x)) List#{x} else List#{}

    pub def isStrBy(f: String -> Bool): JsonFilter = x -> 
        match JsonValue.getString(x) {
            case Some(s) if f(s)    => List#{x}
            case _                  => List#{}
        }
    pub def isNum(): JsonFilter = x -> 
        if (JsonValue.isNumber(x)) List#{x} else List#{}

    // pub def isNumBy(f: String -> Bool): JsonFilter = x -> 
    //     match JsonValue.getString(x) {
    //         case Some(s) if f(s)    => List#{x}
    //         case _                  => List#{}
    //     }

    pub def isBool(): JsonFilter = x -> 
        if (JsonValue.isTrue(x) or JsonValue.isFalse(x)) List#{x} else List#{}

    pub def isTrue(): JsonFilter = x -> 
        if (JsonValue.isTrue(x)) List#{x} else List#{}

    pub def isFalse(): JsonFilter = x -> 
        if (JsonValue.isFalse(x)) List#{x} else List#{}

    pub def isNull(): JsonFilter = x -> 
        if (JsonValue.isNull(x)) List#{x} else List#{}

    pub def isAtomic(): JsonFilter = x -> 
        if (JsonValue.isNull(x) or JsonValue.isString(x) 
            or JsonValue.isTrue(x) or JsonValue.isFalse(x)
            or JsonValue.isNumber(x)) 
            List#{x} 
        else List#{}

    // def getChildren(): JsonFilter = x -> 
    //     if (JsonValue.isArray(x)) 

    //         List#{x} 
    //     else List#{}


    pub def sequence(f: JsonFilter, g: JsonFilter): JsonFilter = x ->
        List.flatten(List.map(g, f(x)))


    pub def concat(f: JsonFilter, g: JsonFilter): JsonFilter = x ->
        f(x) ::: g(x)


    pub def orElse(f: JsonFilter, g: JsonFilter): JsonFilter = x ->
        match f(x) {
            case Nil => g(x)
            case xs  => xs
        }

    pub def when(f: JsonFilter, g: JsonFilter): JsonFilter = x ->
        match g(x) {
            case Nil => List#{x}
            case _   => f(x)
        }

    pub def guards(f: JsonFilter, g: JsonFilter): JsonFilter = x ->
        match f(x) {
            case Nil => List#{}
            case _   => g(x)
        }

}

