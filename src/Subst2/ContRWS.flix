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



namespace Subst2/ContRWS {



    ///
    /// ContRWS is the Reader-Writer-State monad implemented in CPS. 
    ///
    /// We use an Impure continuation so we can internally cast the `ef` of operations and not lose
    /// impurity information.
    ///
    pub enum ContRWS[ka, r, w, s, a](r -> s -> w -> (a -> s -> w -> ka & Impure) -> ka & Impure)



    ///
    /// Helper function for ContRWS's implementations of `map` and `ap`.
    ///
    def apply1(ma: ContRWS[ka, r, w, s, a], r: r, s: s, w: w, k: a -> s -> w -> ka & Impure): ka & Impure =
        let ContRWS(f) = ma;
        f(r, s, w, k)


    ///
    /// Returns the result of `ma`.
    ///
    pub def runContRWS(ma: ContRWS[(a, s, w), r, w, s, a], r: r, s: s): (a, s, w) & Impure with Monoid[w] =
        let ContRWS(f) = ma;
        let cont = (a, s, w) -> (a, s, w) as & Impure;
        f(r, s, Monoid.empty(), cont)

    ///
    /// Returns answer and output `ma`.
    ///
    pub def evalContRWS(ma: ContRWS[(a, s, w), r, w, s, a], r: r, s: s): (a, w) & Impure with Monoid[w] =
        let (a, _, w) = runContRWS(ma, r, s);
        (a, w)


    ///
    /// Returns final state and output `ma`.
    ///
    pub def execContRWS(ma: ContRWS[(a, s, w), r, w, s, a], r: r, s: s): (s, w) & Impure with Monoid[w] =
        let (_, s1, w) = runContRWS(ma, r, s);
        (s1, w)


    instance Functor[ContRWS[ka, r, w, s]] {
        pub def map(f: a -> b & ef, ma: ContRWS[ka, r, w, s, a]): ContRWS[ka, r, w, s, b] & ef =
            ContRWS((r, s, w, k) ->
                apply1(ma, r, s, w, (a1, s1, w1) -> k(f(a1) as & Impure, s1, w1))
            ) as & ef
    }


    instance Applicative[ContRWS[ka, r, w, s]] {
        pub def point(x: a): ContRWS[ka, r, w, s, a] =
            ContRWS((_, s, w, k) -> k(x, s, w))

        pub def ap(mf: ContRWS[ka, r, w, s, a -> b & ef], ma: ContRWS[ka, r, w, s, a]): ContRWS[ka, r, w, s, b] & ef =
            ContRWS((r, s, w, k) ->
                apply1(mf, r, s, w, (f1, s1, w1) ->
                    apply1(ma, r, s1, w1, (a1, s2, w2) ->
                        k(f1(a1) as & Impure, s2, w2)))
            ) as & ef
    }


    instance Monad[ContRWS[ka, r, w, s]] {
        pub def flatMap(f: a -> ContRWS[ka, r, w, s, b] & ef, ma: ContRWS[ka, r, w, s, a]): ContRWS[ka, r, w, s, b] & ef =
            flatMap(f, ma)
    }

    pub def flatMap(f: a -> ContRWS[ka, r, w, s, b] & ef, ma: ContRWS[ka, r, w, s, a]): ContRWS[ka, r, w, s, b] & ef =
        ContRWS((r, s, w, k) ->
            apply1(ma, r, s, w, (a1, s1, w1) ->
                apply1(f(a1) as & Impure, r, s1, w1, (b1, s2, w2) ->
                    k(b1, s2, w2)))
        ) as & ef

    pub def tell(a: w): ContRWS[ka, r, w, s, Unit] with SemiGroup[w] = 
        ContRWS((_, s, w, k) ->
            k(() as & Impure, s, w ++ a)
        )


    pub def ask(): ContRWS[ka, r, w, s, r] = 
        ContRWS((r, s, w, k) ->
            k(r as & Impure, s, w)
        )

    pub def asks(f: r -> a & ef): ContRWS[ka, r, w, s, a] & ef =
        ContRWS((r, s, w, k) ->
            k(f(r) as & Impure, s, w)
        ) as & ef

    pub def local(f: r -> r, ma: ContRWS[ka, r, w, s, a]): ContRWS[ka, r, w, s, a] & ef = 
        ContRWS((r, s, w, k) ->
            apply1(ma, f(r), s, w, a1 -> 
                k(a1))
        ) as & ef

    pub def update(f: s -> s & ef): ContRWS[ka, r, w, s, Unit] & ef = 
        ContRWS((_, s, w, k) ->
            k((), f(s) as & Impure, w)
        ) as & ef

    pub def put(st: s): ContRWS[ka, r, w, s, Unit]  = 
        ContRWS((_, _, w, k) ->
            k((), st, w)
        )

    pub def get(): ContRWS[ka, r, w, s, s] =  
        ContRWS((_, s, w, k) ->
            k(s, s, w)
        )

    pub def gets(proj: s -> a & ef): ContRWS[ka, r, w, s, a] & ef =  
        ContRWS((_, s, w, k) ->
            k(proj(s) as & Impure, s, w)
        ) as & ef

}
