{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Pizza.Lib.Data.Vector.Generic.SizedTests where

import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import qualified Data.Vector as V
import qualified Data.Vector.Generic.Sized as VSized
import Pizza.Core
import Pizza.Lib.Data.Vector.Generic.Sized ()
import Pizza.TestUtils.Mergeable
import Pizza.TestUtils.SBool
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

sizedTests :: TestTree
sizedTests =
  let v3a =
        ( VSized.cons (SSBool "a") $
            VSized.cons (SSBool "b") $
              VSized.cons (SSBool "c") VSized.empty ::
            VSized.Vector V.Vector 3 SBool
        )
      v3b =
        ( VSized.cons (SSBool "d") $
            VSized.cons (SSBool "e") $
              VSized.cons (SSBool "f") VSized.empty ::
            VSized.Vector V.Vector 3 SBool
        )
      l3a = [SSBool "a", SSBool "b", SSBool "c"]
      l3b = [SSBool "d", SSBool "e", SSBool "f"]
      v2c = (VSized.cons (CBool True) $ VSized.cons (CBool False) VSized.empty :: VSized.Vector V.Vector 2 SBool)
      v2s = (VSized.cons (CBool True) $ VSized.cons (SSBool "a") VSized.empty :: VSized.Vector V.Vector 2 SBool)
      v2cc =
        ( VSized.cons True $ VSized.cons False VSized.empty ::
            VSized.Vector V.Vector 2 Bool
        )
   in testGroup
        "SizedTests"
        [ testGroup
            "Mergeable for Sized Vector"
            [ testCase "Zero length" $ do
                testMergeableSimpleEquivClass
                  (VSized.empty :: VSized.Vector V.Vector 0 Integer)
                  []
                  [(SSBool "a", VSized.empty, VSized.empty, VSized.empty)],
              testCase "Simple mergeable contents" $ do
                testMergeableSimpleEquivClass
                  (VSized.cons (SSBool "a") (VSized.cons (SSBool "b") VSized.empty) :: VSized.Vector V.Vector 2 SBool)
                  []
                  [ ( SSBool "a",
                      VSized.cons (SSBool "b") (VSized.cons (SSBool "c") VSized.empty),
                      VSized.cons (SSBool "d") (VSized.cons (SSBool "e") VSized.empty),
                      VSized.cons
                        (ITE (SSBool "a") (SSBool "b") (SSBool "d"))
                        (VSized.cons (ITE (SSBool "a") (SSBool "c") (SSBool "e")) VSized.empty)
                    )
                  ],
              testCase "BuildStrategyList for Sized Vector" $ do
                case buildStrategyList @SBool @Integer
                  mergingStrategy
                  (VSized.cons 1 (VSized.cons 2 (VSized.cons 3 VSized.empty)) :: VSized.Vector V.Vector 3 Integer) of
                  StrategyList idxs _ -> do
                    idxs
                      @=? VSized.cons
                        [DynamicSortedIdx (1 :: Integer)]
                        ( VSized.cons
                            [DynamicSortedIdx (2 :: Integer)]
                            ( VSized.singleton
                                [DynamicSortedIdx (3 :: Integer)]
                            )
                        ),
              testProperty "Ordered mergeable contents" $
                ioProperty . \(x, y, z) -> do
                  let v = VSized.cons x (VSized.cons y (VSized.cons z VSized.empty)) :: VSized.Vector V.Vector 3 Integer
                  testMergeableSimpleEquivClass
                    v
                    [DynamicSortedIdx $ buildStrategyList @SBool mergingStrategy v]
                    [(SSBool "a", v, v, v)],
              testCase "Complex ordered type contents" $ do
                let v1 = VSized.cons Nothing (VSized.singleton Nothing) :: VSized.Vector V.Vector 2 (Maybe SBool)
                let v2 = VSized.cons (Just $ SSBool "a") (VSized.singleton Nothing) :: VSized.Vector V.Vector 2 (Maybe SBool)
                let v2' = VSized.cons (Just $ SSBool "b") (VSized.singleton Nothing) :: VSized.Vector V.Vector 2 (Maybe SBool)
                let v3 = VSized.cons Nothing (VSized.singleton (Just $ SSBool "a")) :: VSized.Vector V.Vector 2 (Maybe SBool)
                let v3' = VSized.cons Nothing (VSized.singleton (Just $ SSBool "b")) :: VSized.Vector V.Vector 2 (Maybe SBool)
                let v4 = VSized.cons (Just $ SSBool "a") (VSized.singleton (Just $ SSBool "b")) :: VSized.Vector V.Vector 2 (Maybe SBool)
                let v4' = VSized.cons (Just $ SSBool "c") (VSized.singleton (Just $ SSBool "d")) :: VSized.Vector V.Vector 2 (Maybe SBool)
                testMergeableSimpleEquivClass
                  v1
                  [DynamicSortedIdx $ buildStrategyList @SBool mergingStrategy v1]
                  [(SSBool "a", v1, v1, v1)]
                testMergeableSimpleEquivClass
                  v2
                  [DynamicSortedIdx $ buildStrategyList @SBool mergingStrategy v2]
                  [(SSBool "c", v2, v2', VSized.cons (Just $ ITE (SSBool "c") (SSBool "a") (SSBool "b")) (VSized.singleton Nothing))]
                testMergeableSimpleEquivClass
                  v3
                  [DynamicSortedIdx $ buildStrategyList @SBool mergingStrategy v3]
                  [(SSBool "c", v3, v3', VSized.cons Nothing (VSized.singleton (Just $ ITE (SSBool "c") (SSBool "a") (SSBool "b"))))]
                testMergeableSimpleEquivClass
                  v4
                  [DynamicSortedIdx $ buildStrategyList @SBool mergingStrategy v4]
                  [ ( SSBool "e",
                      v4,
                      v4',
                      VSized.cons
                        (Just $ ITE (SSBool "e") (SSBool "a") (SSBool "c"))
                        (VSized.singleton (Just $ ITE (SSBool "e") (SSBool "b") (SSBool "d")))
                    )
                  ]
            ],
          testGroup
            "SimpleMergeable for Sized Vector"
            [ testCase "Zero length" $ do
                mrgIte
                  (SSBool "a")
                  (VSized.empty :: VSized.Vector V.Vector 0 Integer)
                  (VSized.empty :: VSized.Vector V.Vector 0 Integer)
                  @=? (VSized.empty :: VSized.Vector V.Vector 0 Integer),
              testCase "Simple mergeable contents" $ do
                mrgIte
                  (SSBool "a")
                  (VSized.cons (SSBool "b") (VSized.cons (SSBool "c") VSized.empty) :: VSized.Vector V.Vector 2 SBool)
                  (VSized.cons (SSBool "d") (VSized.cons (SSBool "e") VSized.empty) :: VSized.Vector V.Vector 2 SBool)
                  @=? ( VSized.cons
                          (ITE (SSBool "a") (SSBool "b") (SSBool "d"))
                          (VSized.cons (ITE (SSBool "a") (SSBool "c") (SSBool "e")) VSized.empty) ::
                          VSized.Vector V.Vector 2 SBool
                      )
            ],
          testCase "EvaluateSym for Sized Vector" $ do
            let model = M.fromList [(SSymbol "a", True), (SSymbol "b", False)]
            evaluateSym
              False
              model
              ( VSized.cons (SSBool "a") $ VSized.cons (SSBool "c") $ VSized.cons (SSBool "b") VSized.empty ::
                  VSized.Vector V.Vector 3 SBool
              )
              @=? VSized.cons (CBool True) (VSized.cons (SSBool "c") $ VSized.cons (CBool False) VSized.empty)
            evaluateSym
              True
              model
              ( VSized.cons (SSBool "a") $ VSized.cons (SSBool "c") $ VSized.cons (SSBool "b") VSized.empty ::
                  VSized.Vector V.Vector 3 SBool
              )
              @=? VSized.cons (CBool True) (VSized.cons (CBool False) $ VSized.cons (CBool False) VSized.empty),
          testCase "ExtractSymbolics for Sized Vector" $ do
            extractSymbolics
              ( VSized.cons (SSBool "a") $ VSized.cons (SSBool "c") $ VSized.cons (SSBool "b") VSized.empty ::
                  VSized.Vector V.Vector 3 SBool
              )
              @=? S.fromList [SSymbol "a", SSymbol "b", SSymbol "c"],
          testCase "SEq for Sized Vector" $ do
            v3a ==~ v3b @=? (l3a ==~ l3b :: SBool),
          testCase "SOrd for Sized Vector" $ do
            v3a <=~ v3b @=? (l3a <=~ l3b :: SBool)
            v3a <~ v3b @=? (l3a <~ l3b :: SBool)
            v3a >=~ v3b @=? (l3a >=~ l3b :: SBool)
            v3a >~ v3b @=? (l3a >~ l3b :: SBool)
            v3a `symCompare` v3b @=? (l3a `symCompare` l3b :: UnionMBase SBool Ordering),
          testCase "ToCon for Sized Vector" $ do
            toCon v2c @=? Just v2cc
            toCon v2s @=? (Nothing :: Maybe (VSized.Vector V.Vector 2 Bool)),
          testCase "ToSym for Sized Vector" $ do
            toSym v2cc @=? v2c
        ]
