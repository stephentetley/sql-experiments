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


namespace Ceri/Monad {


    pub enum Mon[a](Int32 -> (a, Int32))

    def apply1(m: Mon[a], s: Int32): (a, Int32) = 
        let Mon(m1) = m;
        m1(s)

    pub def point(x: a): Mon[a] = 
        Mon(s -> (x, s))
    
    pub def map(f: a -> b, m: Mon[a]): Mon[b] = 
        Mon(s -> {
            let (x, s1) = apply1(m, s);
            (f(x), s1)
        })

    pub def bind(m: Mon[a], k: a -> Mon[b]): Mon[b] = 
        Mon(s -> {
            let (x, s1) = apply1(m, s);
            apply1(k(x), s1)
        })

    pub def flatMap(k: a -> Mon[b], m: Mon[a]): Mon[b] = bind(m, k)

    pub def newvar(): Mon[String] = Mon(s -> ("N{$s}", s + 1))


}