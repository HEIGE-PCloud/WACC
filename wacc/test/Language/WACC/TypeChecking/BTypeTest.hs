{- AUTOCOLLECT.TEST -}

module Language.WACC.TypeChecking.BTypeTest
  (
  {- AUTOCOLLECT.TEST.export -}
  )
where

import Language.WACC.TypeChecking.BType
import Test

test =
  testGroup
    "unify"
    [ testProperty "is reflexive" $ \t -> unify t t == Just t
    , testGroup
        "always succeeds"
        [ testProperty "when BAny is provided" $ \t -> unify BAny t == Just t
        , testProperty "when BAny is expected" $ \t -> unify t BAny == Just t
        , testProperty "when BUnknown is provided" $
            \t -> unify BUnknown t == Just (if t == BAny then BUnknown else t)
        , testProperty "when BUnknown is expected" $
            \t -> unify t BUnknown == Just (if t == BAny then BUnknown else t)
        ]
    , testGroup
        "distributes"
        [ testProperty "through arrays" $
            \t1 t2 -> unify (BArray t1) (BArray t2) == (BArray <$> unify t1 t2)
        , testProperty "through known pairs (left)" $
            \lt1 lt2 rt ->
              unify (BKnownPair lt1 rt) (BKnownPair lt2 rt)
                == (flip BKnownPair rt <$> unify lt1 lt2)
        , testProperty "through known pairs (right)" $
            \lt rt1 rt2 ->
              unify (BKnownPair lt rt1) (BKnownPair lt rt2)
                == (BKnownPair lt <$> unify rt1 rt2)
        ]
    , testGroup
        "char[] and string"
        [ testCase "char[] implicitly casts to string" $
            unify (BArray BChar) BString @?= Just BString
        , testCase "string does not cast to char[]" $
            unify BString (BArray BChar) @?= Nothing
        ]
    ]
