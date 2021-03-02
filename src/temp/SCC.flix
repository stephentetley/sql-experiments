/*
 * Copyright 2021 Stephen Tetley
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


namespace SCC {

    type alias Vertex = Int32
    
    pub enum Edge { 
        case Edge(Int32, Int32)
    }

    enum NodeState {
        case Unvisited
        case Visited
        case Finished
        case Popped
    }

    instance Eq[NodeState] { 
        def eq(x: NodeState, y: NodeState): Bool = match (x,y) { 
            case (Unvisited, Unvisited) => true
            case (Visited, Visited)     => true
            case (Finished, Finished)   => true
            case (Popped, Popped)       => true
            case (_, _)                 => false
        }

        def neq(x: NodeState, y: NodeState): Bool = not (x == y)
    }

    type alias State = {nodeState: Array[NodeState], dfsLow: Array[Int32], dfsNum: Array[Int32]}

    pub def scc(vs: List[Int32], es: List[Edge]): List[List[Int32]] = 
        sccHelper(vs, es) as & Pure

    def sccHelper(vs: List[Int32], es: List[Edge]): List[List[Vertex]] & Impure = 
        let len = List.length(vs);
        let s1 = {nodeState = Array.new(Unvisited, len), dfsLow = Array.new(-1, len), dfsNum = Array.new(-1, len)};
        let (_, ans) = List.mapAccumLeft((stk, x1) -> strongconnect(s1, ref 0, x1, es, stk), Nil, vs);
        List.map(fst, ans) |> List.flatten |> List.filter(xs -> not (List.isEmpty(xs))) 

    

    def strongconnect(s1: State, num: Ref[Int32], v: Int32, es: List[Edge], stack: List[Int32]): (List[Int32], (List[List[Int32]], Int32)) & Impure = 
        if (Array.get(s1.nodeState, v) != Unvisited) 
            (stack, (Nil, Array.get(s1.dfsNum, v)))
        else {
            Array.put(s1.nodeState, v, Visited);
            let num1 = deref num + 1;
            num := num1;
            Array.put(s1.dfsNum, v, num1);
            Array.put(s1.dfsLow, v, num1);
            let stack1 = v :: stack;
            let ws = getSuccessors(v, es);
            let step = (stk, w) -> { 
                if (Array.get(s1.nodeState, v) == Popped) 
                    (stk, Nil)
                else {
                    let (stk1, (kids, wNum)) = strongconnect(s1, num, w, es, stk);
                    let prevLow = Array.get(s1.dfsLow, v);
                    Array.put(s1.dfsLow, v, Int32.min(prevLow, wNum));
                    (stk1, kids)
                }
            };
            let (stack2, kids) = List.mapAccumLeft(step , stack1, ws);
            let kidsOut = kids |> List.flatten |> List.filter(xs -> not (List.isEmpty(xs)));
            Array.put(s1.nodeState, v, Finished);
            let kids1 = {
                if (Array.get(s1.dfsLow, v) >= Array.get(s1.dfsNum, v)) {
                    Nil /// unstack(s1, stack2)
                }
                else Nil

            };
            (stack2, (kids1 :: kidsOut, Array.get(s1.dfsLow, v)))

        }

    def getSuccessors(v: Int32, es: List[Edge]): List[Int32] = 
        let step = x -> match x {
            case Edge(a, b) => if (a == v) Some(b) else None
        };
        List.filterMap(step, es)

}