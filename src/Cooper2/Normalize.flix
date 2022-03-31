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


namespace Cooper2/Normalize {

    use Applicative.{point};

    use Cooper2/Monad.Mon;
    use Cooper2/Monad.{flatMap};

    use Cooper2/SourceLanguage.SLTerm;
    use Cooper2/SourceLanguage.SLTerm.{SLFor, SLIfThenElse,
        SLTable, SLBagSingleton, SLBagEmpty, SLBagUnion,
        SLRecord, SLFieldProjection, SLApply, SLLambda,
        SLVariable, SLConstant, SLPrim, SLEmpty, 
        SLQuery};
    use Cooper2/SourceLanguage.SLType;
    use Cooper2/SourceLanguage.SLType.{TyRecord, TyBag,
        TyFun, TyBool, TyInt, TyString};


    pub def forZeroL(x: SLTerm, _: SLType): Mon[SLTerm] = match x { 
        case SLFor(_, SLBagEmpty, _)                => point(SLBagEmpty)
        case _                                      => point(x)
    }

    pub def forZeroR(x: SLTerm, _: SLType): Mon[SLTerm] = match x { 
        case SLFor(_, _, SLBagEmpty)                => point(SLBagEmpty)
        case _                                      => point(x)
    }

    pub def ifZero(x: SLTerm, _: SLType): Mon[SLTerm] = match x { 
        case SLIfThenElse(_, SLBagEmpty, SLBagEmpty)    => point(SLBagEmpty)
        case _                                          => point(x)
    }
 
    pub def ifUnion(x: SLTerm, _: SLType): Mon[SLTerm] = match x { 
        case SLIfThenElse(b, SLBagUnion(m, n), SLBagEmpty)  => 
            point(SLBagUnion(SLIfThenElse(b, m, SLBagEmpty), SLIfThenElse(b, n, SLBagEmpty)))

        case _                                              => point(x)
    }

    pub def ignoreDb(x: SLTerm, _: SLType): Mon[SLTerm] = match x { 
        case SLQuery(m)                             => point(m)
        case _                                      => point(x)
    }
 
}