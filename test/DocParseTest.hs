{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use $>" #-}
module DocParseTest where

import Control.Applicative
import Data.Char qualified as Char
import Data.Functor qualified
import Data.List
import Display
import DocParse
import Parse (Parse)
import Parse qualified as P
import Syntax
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck
import Test.QuickCheck qualified as QC

test_wsP :: Test
test_wsP =
  TestList
    [ P.parse (wsP P.alpha) "a" ~?= Right 'a',
      P.parse (many (wsP P.alpha)) "a b \n   \t c" ~?= Right "abc"
    ]

test_nlP :: Test
test_nlP =
  TestList
    [ P.parse (nlP P.alpha) "a" ~?= Right 'a',
      P.parse (many (nlP P.alpha)) "ab \n   \t c" ~?= Right "ab"
    ]

testStringsP :: Test
testStringsP =
  TestList
    [ P.parse (stringsP ["public"]) "public class Foo {}" ~?= Right "public",
      P.parse (stringsP ["private"]) "private class Foo {}" ~?= Right "private",
      P.parse (stringsP ["private", "public"]) "private class Foo {}" ~?= Right "private",
      P.parse (stringsP []) "public class Foo {}" ~?= Right "",
      P.parse (stringsP ["public", "private"]) "class Foo {}" ~?= Right ""
    ]

test_tagP :: Test
test_tagP =
  TestList
    [ P.parse tagP "@param x" ~?= Right (Param (Name "x") (Description "")),
      P.parse tagP "@param x the x value" ~?= Right (Param (Name "x") (Description "the x value")),
      P.parse tagP "@param x the x value\n" ~?= Right (Param (Name "x") (Description "the x value"))
    ]

test_classP :: Test
test_classP =
  TestList
    [ P.parse classP "class Foo {}" ~?= Right (Class (JavaDocHeader (Description "") []) (Name "Foo")),
      P.parse classP "public class Foo {}" ~?= Right (Class (JavaDocHeader (Description "") []) (Name "Foo")),
      P.parse classP "private class Foo { }" ~?= Right (Class (JavaDocHeader (Description "") []) (Name "Foo")),
      P.parse classP "class Foo { /* blah */ }" ~?= Right (Class (JavaDocHeader (Description "") []) (Name "Foo")),
      P.parse classP "class Foo { \n // blah \n }" ~?= Right (Class (JavaDocHeader (Description "") []) (Name "Foo")),
      P.parse classP "class Foo { \n // blah \n } \n" ~?= Right (Class (JavaDocHeader (Description "") []) (Name "Foo")),
      P.parse classP "/**\n * The Foo class\n */\nclass Foo { \n // blah \n } \n" ~?= Right (Class (JavaDocHeader (Description "The Foo class") []) (Name "Foo")),
      P.parse classP "/**\n * The Foo class\n * @version 1.0\n */ \n class Foo { \n // blah \n } \n" ~?= Right (Class (JavaDocHeader (Description "The Foo class") [Version (Description "1.0")]) (Name "Foo")),
      P.parse classP "/**\n * The Foo class\n * @version 1.0\n * @param x the x value\n*/ \n class Foo { \n // blah \n } \n" ~?= Right (Class (JavaDocHeader (Description "The Foo class") [Version (Description "1.0"), Param (Name "x") (Description "the x value")]) (Name "Foo"))
    ]

test_methodP :: Test
test_methodP =
  TestList
    [ P.parse methodP "void bar();" ~?= Right (Method (JavaDocHeader (Description "") []) (Name "bar")),
      P.parse methodP "public void bar(){}" ~?= Right (Method (JavaDocHeader (Description "") []) (Name "bar")),
      P.parse methodP "private void bar();" ~?= Right (Method (JavaDocHeader (Description "") []) (Name "bar")),
      P.parse methodP "private void bar(){ if (true) {}else{} \n return \n that;}" ~?= Right (Method (JavaDocHeader (Description "") []) (Name "bar")),
      P.parse methodP "void bar(); // comment here" ~?= Right (Method (JavaDocHeader (Description "") []) (Name "bar")),
      P.parse methodP "/**\n* The bar method\n* @version 1.0\n*/\nvoid bar();" ~?= Right (Method (JavaDocHeader (Description "The bar method") [Version (Description "1.0")]) (Name "bar")),
      P.parse methodP "public static void bar(){}" ~?= Right (Method (JavaDocHeader (Description "") []) (Name "bar"))
    ]

test_interfaceP :: Test
test_interfaceP =
  TestList
    [ P.parse interfaceP "interface Foo {}" ~?= Right (Interface (JavaDocHeader (Description "") []) (Name "Foo")),
      P.parse interfaceP "public interface Foo {}" ~?= Right (Interface (JavaDocHeader (Description "") []) (Name "Foo")),
      P.parse interfaceP "private interface Foo { }" ~?= Right (Interface (JavaDocHeader (Description "") []) (Name "Foo")),
      P.parse interfaceP "interface Foo { /* blah */ }" ~?= Right (Interface (JavaDocHeader (Description "") []) (Name "Foo")),
      P.parse interfaceP "interface Foo { \n // blah \n }" ~?= Right (Interface (JavaDocHeader (Description "") []) (Name "Foo")),
      P.parse interfaceP "interface Foo { \n // blah \n } \n" ~?= Right (Interface (JavaDocHeader (Description "") []) (Name "Foo")),
      P.parse interfaceP "/**\n* The Foo interface\n* @version 1.0\n*/\ninterface Foo { \n // blah \n } \n" ~?= Right (Interface (JavaDocHeader (Description "The Foo interface") [Version (Description "1.0")]) (Name "Foo")),
      P.parse interfaceP "/**\n* The Foo interface\n* @version 1.0\n * @param x the x value\n*/\ninterface Foo { \n // blah \n } \n" ~?= Right (Interface (JavaDocHeader (Description "The Foo interface") [Version (Description "1.0"), Param (Name "x") (Description "the x value")]) (Name "Foo"))
    ]

test_interfaceAndMethodP :: Test
test_interfaceAndMethodP =
  TestList
    [ P.parse interfaceAndMethodP "public interface Test {\n}" ~?= Right [Interface (JavaDocHeader (Description "") []) (Name "Test")]
    ]

test_enumP :: Test
test_enumP =
  TestList
    [ P.parse enumP "enum Foo {}" ~?= Right (Enum (Description "") (Name "Foo")),
      P.parse enumP "public enum Foo {}" ~?= Right (Enum (Description "") (Name "Foo")),
      P.parse enumP "private enum Foo { }" ~?= Right (Enum (Description "") (Name "Foo")),
      P.parse enumP "enum Foo { /* blah */ }" ~?= Right (Enum (Description "") (Name "Foo")),
      P.parse enumP "enum Foo { \n // blah \n }" ~?= Right (Enum (Description "") (Name "Foo")),
      P.parse enumP "enum Foo { \n // blah \n } \n" ~?= Right (Enum (Description "") (Name "Foo"))
    ]

packageP :: Parse String
packageP = wsP (stringsP ["package", "import"]) *> wsP (many (P.satisfy (/= ';'))) <* wsP (P.string ";")

test_packageP :: Test
test_packageP =
  TestList
    [ P.parse packageP "package org.cis1200;" ~?= Right "org.cis1200",
      P.parse packageP "import org.cis1200.*;" ~?= Right "org.cis1200.*"
    ]

test_javaDocP :: Test
test_javaDocP =
  TestList
    [ P.parse javaDocP "public class Foo {\n/**\n* The Foo class\n* @version 1.0\n*/\npublic void bar() {\n}\n}" ~?= Right (JavaDoc [Class (JavaDocHeader (Description "") []) (Name "Foo"), Method (JavaDocHeader (Description "The Foo class") [Version (Description "1.0")]) (Name "bar")])
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

-- Roundtrip tests: Given JavaDoc represented in Haskell syntax, translate it back into Java string representation.

-- | Generate string version of tags from their object representation
generateTagsText :: [Tag] -> String
generateTagsText = concatMap generateTagText
  where
    generateTagText :: Tag -> String
    generateTagText tag = case tag of
      Author (Description d) -> " * @author " ++ d ++ "\n"
      Version (Description d) -> " * @version " ++ d ++ "\n"
      Param (Name n) (Description d) -> " * @param " ++ n ++ " " ++ d ++ "\n"
      Return (Description d) -> " * @return " ++ d ++ "\n"
      Deprecated (Description d) -> " * @deprecated " ++ d ++ "\n"
      Throws (Name n) (Description d) -> " * @throws " ++ n ++ " " ++ d ++ "\n"

-- >>> generateJavaDocCommentText (Class (JavaDocHeader (Description "The Foo class\n") [Version (Description "1.0")]) (Name "Foo"))

-- | Create an arbitrary comment string based on JavaDocComment Object
generateJavaDocCommentText :: JavaDocComment -> String
generateJavaDocCommentText jdc = case jdc of
  Class (JavaDocHeader (Description d) tags) (Name n) ->
    "/**\n" ++ " * " ++ d ++ "\n" ++ generateTagsText tags ++ "*/\n" ++ "public class " ++ n ++ " {\n}\n"
  Interface (JavaDocHeader (Description d) tags) (Name n) ->
    "/**\n" ++ " * " ++ d ++ "\n" ++ generateTagsText tags ++ "*/\n" ++ "public interface " ++ n ++ " {\n}\n"
  Enum (Description d) (Name n) ->
    "/**\n" ++ " * " ++ d ++ "*/\n" ++ "public enum " ++ n ++ " {\n}\n"
  Method (JavaDocHeader (Description d) tags) (Name n) ->
    "/**\n" ++ " * " ++ d ++ "\n" ++ generateTagsText tags ++ "*/\n" ++ "public void " ++ n ++ "() {\n}\n"