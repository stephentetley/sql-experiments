/*
 * Copyright 2022 Stephen Tetley
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



namespace Subst2/Mon {

    use Functor.{<$>};
    use Applicative.{point};
    use Subst2/ContRWS.{flatMap};

    pub type alias Env = Map[String, String]

    pub type alias Mon[ka, a] = Subst2/ContRWS.ContRWS[ka, Env, Unit, Int32, a]


    pub def runMon(env: Env, ma: Mon[(a, Int32, Unit), a]): a & Impure = 
        let (a, _) = Subst2/ContRWS.evalContRWS(ma, env, 0);
        a

    pub def flatMap(f: a -> Mon[ka, b] & ef, ma: Mon[ka, a]): Mon[ka, b] & ef = Subst2/ContRWS.flatMap(f, ma)

    pub def local(f: Env -> Env, ma: Mon[ka, a]): Mon[ka, a] & ef = Subst2/ContRWS.local(f, ma)


    pub def find(s: String): Mon[ka, String] = 
        Subst2/ContRWS.asks(r -> 
            match Map.get(s, r) { 
                case Some(x) => x
                case None    => s
            })


    def freshInt32(): Mon[ka, Int32] = 
        let* i = Subst2/ContRWS.get();
        let* _ = Subst2/ContRWS.update(s -> s + 1);
        point(i)

    pub def genId(s: String): Mon[ka, String] = 
        (i -> "${s}.${i}") <$> freshInt32()
        

        

}
