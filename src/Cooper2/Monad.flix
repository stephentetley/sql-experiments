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


namespace Cooper2/Monad {

    use Cooper2/SourceLanguage.SLTerm;
    use Cooper2/SourceLanguage.SLTerm.{SLFor, SLIfThenElse,
        SLTable, SLBagSingleton, SLBagEmpty, SLBagUnion,
        SLRecord, SLFieldProjection, SLApply, SLLambda,
        SLVariable, SLConstant, SLPrim, SLEmpty, 
        SLQuery};
    use Cooper2/SourceLanguage.{SLVariable};

    pub type alias Substs = Map[SLVariable, SLTerm]

    /// Environment, integer supply and failure
    pub opaque type Mon[a] = Substs -> Int32 -> Result[(Int32, a), String]

    instance Functor[Mon] {
        pub def map(f: a -> b & ef, ma: Mon[a]): Mon[b] & ef = map(f, ma)
    }

    instance Applicative[Mon] {
        pub def point(x: a): Mon[a] = point(x)
        pub def ap(mf: Mon[a -> b & ef], ma: Mon[a]): Mon[b] & ef = ap(mf, ma)
    }

    instance Monad[Mon] {
        pub def flatMap(k: a -> Mon[b] & ef, ma: Mon[a]): Mon[b] & ef = flatMap(k, ma)
    }

    pub def run(ma: Mon[a]): Result[a, String] = 
        let env = Map.empty();
        apply1(ma, env, 0) |> Result.map(snd)

    def apply1(x: Mon[a], env: Substs, st: Int32): Result[(Int32, a), String] = 
        let Mon(x1) = x;
        x1(env, st)

    pub def point(x: a): Mon[a] = Mon((_, s) -> Ok((s, x)))

    pub def map(f: a -> b & ef, ma: Mon[a]): Mon[b] & ef = 
        Mon((r, s) -> 
            match apply1(ma, r, s) {
                case Ok((s1, a)) => Ok(s1, f(a) as & Pure)
                case Err(msg)    => Err(msg)
            }
        ) as & ef

    pub def ap(mf: Mon[a -> b & ef], ma: Mon[a]): Mon[b] & ef = 
        Mon((r, s) -> 
            match apply1(mf, r, s) {
                case Ok((s1, f)) => match apply1(ma, r, s1) {
                    case Ok((s2, a)) => Ok((s2, f(a) as & Pure))
                    case Err(msg)    => Err(msg)
                }
                case Err(msg)    => Err(msg)
            }
        ) as & ef

    pub def flatMap(k: a -> Mon[b] & ef, ma: Mon[a]): Mon[b] & ef = 
        Mon((r, s) -> 
            match apply1(ma, r, s) {
                case Ok((s1, a)) => apply1(k(a) as & Pure, r, s1)
                case Err(msg)    => Err(msg)
            }
        ) as & ef

    pub def throwError(msg: String): Mon[a] = Mon((_, _) -> Err(msg))

    pub def newVar(sn: String): Mon[String] = 
        Mon((_, s) -> let s1 = s+1 ; Ok((s1, sn + ".${s1}")))

    pub def lookup(varname: String): Mon[SLTerm] = 
        Mon((r, s) -> 
            match Map.get(varname, r) {
                case None    => Ok((s, SLVariable(varname)))
                case Some(t) => Ok((s, t))
            }
        )
         
    pub def subst(t: SLTerm): Mon[SLTerm] = match t {
        case SLVariable(v)                          => lookup(v)
        case SLConstant(_)                          => point(t)
        case _                                      => point(t)
    }

}