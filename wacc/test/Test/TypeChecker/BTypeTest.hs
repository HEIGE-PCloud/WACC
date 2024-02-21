{- AUTOCOLLECT.TEST -}

module Test.TypeChecker.BTypeTest
  (
  {- AUTOCOLLECT.TEST.export -}
  )
where

import Language.WACC.TypeChecking.BType
import Test.TypeChecker.Arbitrary ()
import Test

unifyNoCast :: BType -> BType -> Maybe BType
unifyNoCast (BArray _) BString = Nothing
unifyNoCast bt1 bt2 = unify bt1 bt2

test =
  testGroup
    "unitTests"
    [ testGroup
        "unify"
        [ testProperty "is reflexive" $ \t -> unify t t == Just t
        , testGroup
            "always succeeds"
            [ testProperty "when BAny is provided" $
                \t -> unify BAny t == Just t
            , testProperty "when BAny is expected" $
                \t -> unify t BAny == Just t
            ]
        , testGroup
            "distributes"
            [ testProperty "through arrays" $
                \t1 t2 ->
                  unify (BArray t1) (BArray t2)
                    == (BArray <$> unifyNoCast t1 t2)
            , testProperty "through known pairs (left)" $
                \lt1 lt2 rt ->
                  unify (BKnownPair lt1 rt) (BKnownPair lt2 rt)
                    == (flip BKnownPair rt <$> unifyNoCast lt1 lt2)
            , testProperty "through known pairs (right)" $
                \lt rt1 rt2 ->
                  unify (BKnownPair lt rt1) (BKnownPair lt rt2)
                    == (BKnownPair lt <$> unifyNoCast rt1 rt2)
            ]
        , testGroup
            "char[] and string"
            [ testCase "char[] implicitly casts to string" $
                unify (BArray BChar) BString @?= Just BString
            , testCase "char[][] does not cast to string[]" $
                unify (BArray (BArray BChar)) (BArray BString) @?= Nothing
            , testCase "string does not cast to char[]" $
                unify BString (BArray BChar) @?= Nothing
            ]
        ]
    ]
