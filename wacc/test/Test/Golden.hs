{- AUTOCOLLECT.TEST -}
module Test.Golden
  (
  {- AUTOCOLLECT.TEST.export -}
  )
where

import Test.Tasty.Golden (findByExtension, goldenVsFile, goldenVsString)

-- goldenTests = do
--   yamlFiles <- findByExtension [".yaml"] "."
--   return $
--     testGroup
--       "YamlToJson golden tests"
--       [ goldenVsString
--           (takeBaseName yamlFile) -- test name
--           jsonFile -- golden file path
--           (yamlToJson <$> LBS.readFile yamlFile) -- action whose result is tested
--         | yamlFile <- yamlFiles,
--           let jsonFile = replaceExtension yamlFile ".json"
--       ]
