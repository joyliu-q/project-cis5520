module Test where

import Test.QuickCheck
import Test.QuickCheck qualified as QC

-- typedef String to JavaDocString so we aren't generating for all strings
newtype JavaDocString = JavaDocString String deriving (Eq, Show)

genClass :: String -> String
genClass body = "public class Test {\n" ++ body ++ "}\n"

-- genMethod :: String -> Gen String
genMethod :: Gen String
genMethod = do
  returnType <- QC.frequency [(1, return "int"), (1, return "String"), (1, return "char"), (1, return "double"), (1, return "Object")]
  funcIndexValue <- QC.chooseInt (1, 1000)
  return $ "public " ++ returnType ++ " func" ++ show funcIndexValue ++ "{}\n"

-- return "public " ++ returnType ++ "func" ++ show ()

genSmallInt :: Gen Int
genSmallInt = QC.chooseInt (1, 5)

genComment :: String -> [String] -> Gen String
genComment desc tags = do
  tagCount <- QC.chooseInt (1, 5)
  formattedTagStr <- genFormattedTag tagCount
  --   output <- "/** \n* " ++ desc ++ "The Foo class\n*\n* @version 1.0\n*/"
  return $ "/** \n*" ++ desc ++ "\n*\n" ++ formattedTagStr

{-
"/** \n* "
      ++ genFormattedTag2
      ++ "The Foo class\n*\n* @version 1.0\n*/\n"
-}

genFormattedTag :: Int -> Gen String
genFormattedTag 0 = QC.frequency [(1, return "")]
genFormattedTag n = do
  tag <- genTag
  tail <- genFormattedTag (n - 1)
  return (" *" ++ tag ++ "\n" ++ tail)

-- genComment :: [Char] -> [String] -> [Char]
-- genComment :: p1 -> p2 -> Gen String
-- genComment desc tags = do
--   x <- genSmallInt
--   return $ genFormattedTag x

genTag :: Gen String
genTag =
  QC.frequency
    [ (1, return "@author this is the author name"),
      (1, return "@deprecated this was deprecated"),
      (1, return "@return this is the return value")
    ]

instance Arbitrary String where
  arbitrary = QC.sized gen
    where
      gen n =
        QC.frequency
          [ (1, return "public class Test {\n    }\n"),
            (1, return "")
          ]

  shrink = error "err"