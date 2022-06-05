/*
 * Copyright 2020 Stephen Tetley
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

namespace Common {

    pub enum StateMonad[s, a](s -> (s, a))
    
}

namespace Common/StateMonad {

    use Common.StateMonad;
    use Common.StateMonad.{StateMonad};

    def apply1(ma: StateMonad[s, a], st: s): (s, a) = 
        let StateMonad(x1) = ma;
        x1(st)

    pub def runState(ma: StateMonad[s, a], st: s): (s, a) = apply1(ma, st)

    pub def evalState(ma: StateMonad[s, a], st: s): a = 
        apply1(ma, st) |> snd

    pub def execState(ma: StateMonad[s, a], st: s): s = 
        apply1(ma, st) |> fst


    pub def return(x: a): StateMonad[s, a] = 
        StateMonad(s -> (s, x))
    
    pub def map(f: a -> b, ma: StateMonad[s, a]): StateMonad[s, b] = 
        StateMonad(s -> 
            let (s1, a) =  apply1(ma, s);
            (s1, f(a))
        )


    pub def ap(mf: StateMonad[s, a -> b], ma: StateMonad[s, a]): StateMonad[s, b] = 
        StateMonad(s -> 
            let (s1, f) =  apply1(mf, s);
            let (s2, a) =  apply1(ma, s1);
            (s2, f(a))
        )

    pub def bind(ma: StateMonad[s, a], fn: a -> StateMonad[s, b]): StateMonad[s, b] = 
        StateMonad(s -> 
            let (s1, a) =  apply1(ma, s);
            let mb = fn(a);
            apply1(mb, s1)
        )

    pub def flatMap(fn: a -> StateMonad[s, b], ma: StateMonad[s, a]): StateMonad[s, b] = bind(ma, fn)


    pub def get(): StateMonad[s, s] = 
        StateMonad(s -> (s, s))

    pub def put(st: s): StateMonad[s, Unit] = 
        StateMonad(_ -> (st, ()))


    pub def liftM(f: a -> b, ma: StateMonad[s, a]): StateMonad[s, b] = map(f, ma)

    pub def liftM2(f: a -> b -> c, ma: StateMonad[s, a], mb: StateMonad[s, b]): StateMonad[s, c] =
        let* a = ma;
        let* b = mb;
        return(f(a,b))


}