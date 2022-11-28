module DocParse where

import Control.Applicative
import Data.Char qualified as Char
import Syntax
import Parse (Parse)
import Parse qualified as P
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck qualified as QC
import qualified Data.Functor

-- prop_roundtrip_val :: Value -> Bool
-- prop_roundtrip_val v = P.parse valueP (pretty v) == Right v

-- prop_roundtrip_exp :: Expression -> Bool
-- prop_roundtrip_exp e = P.parse expP (pretty e) == Right e

-- prop_roundtrip_stat :: Statement -> Bool
-- prop_roundtrip_stat s = P.parse statementP (pretty s) == Right s

-- Helper functions
-- takes a parser, runs it, then skips over any whitespace characters occurring afterwards
wsP :: Parse a -> Parse a
wsP p = p <* many (P.satisfy Char.isSpace)

test_wsP :: Test
test_wsP =
  TestList
    [ P.parse (wsP P.alpha) "a" ~?= Right 'a',
      P.parse (many (wsP P.alpha)) "a b \n   \t c" ~?= Right "abc"
    ]

-- takes a parser, runs it, then returns any 'string' occurring afterwards
stringsP :: [String] -> Parse [Char]
stringsP l = case l of
  [] -> pure []
  hd : tl -> P.string hd <|> stringsP tl

testSkipStringsP :: Test
testSkipStringsP =
  TestList
    [ P.parse (stringsP ["public"]) "public class Foo {}" ~?= Right "public",
      P.parse (stringsP ["private"]) "private class Foo {}" ~?= Right "private",
      P.parse (stringsP ["private", "public"]) "private class Foo {}" ~?= Right "private",
      P.parse (stringsP []) "public class Foo {}" ~?= Right "",
      P.parse (stringsP ["public", "private"]) "class Foo {}" ~?= Right ""
    ]

-- takes a parser, runs it, and returns everything
anyChar = P.satisfy (const True)

-- Tag parsers

-- Parser for the string that comes directly after an @tag

-- @author name-text

-- >>> P.parse authorP "@author John Smith"
-- Right (Author (Description "John Smith"))

-- Given a string "tag", returns a parser for the string that comes directly after the @tag
-- >>> P.parse (descriptionTagP "author") "@author John Smith"
-- Right (Description "John Smith")
descriptionTagP :: String -> Parse Description
descriptionTagP tag  = Description <$> (wsP (P.string tagString) *> wsP (many anyChar))
  where
    tagString = "@" ++ tag

-- Given a string "tag", returns a parser for the Name (first word) and Description (rest of the string) that comes directly after the @tag

nameDescriptionTagP :: String -> (Parse Name, Parse Description)
nameDescriptionTagP tag = do 
  let tagString = "@" ++ tag
  -- name is the first word after the tag
  let name = Name <$> (wsP (P.string tagString) *> wsP (many (P.satisfy Char.isAlphaNum)))
  -- descriptionParser is a parser for the string after both the tag and the name, stripped of any "\n" characters
  let description = Description <$> wsP (many (P.satisfy (/= '\n')))
  (name, description)

tagP :: Parse Tag
tagP = wsP (authorP <|> paramP <|> returnP <|> throwsP <|> versionP)
  where
    authorP :: Parse Tag
    authorP = Author <$> descriptionTagP "author"
    paramP :: Parse Tag
    paramP = Param <$> name <*> description where
      (name, description) = nameDescriptionTagP "param"
    returnP :: Parse Tag
    returnP = Return <$> descriptionTagP "return"
    throwsP :: Parse Tag
    throwsP = Throws <$> name <*> description where
      (name, description) = nameDescriptionTagP "throws"
    versionP :: Parse Tag
    versionP = Version <$> descriptionTagP "version"

test_tagP :: Test
test_tagP =
  TestList
    [ P.parse tagP "@param x" ~?= Right (Param (Name "x") (Description "")),
      P.parse tagP "@param x the x value" ~?= Right (Param (Name "x") (Description "the x value")),
      P.parse tagP "@param x the x value\n" ~?= Right (Param (Name "x") (Description "the x value"))
    ]

-- Extract inner values of a comment. Comment can either by enclosed by /* */ (multiline) or // (single line)
-- >>> P.parse commentP "/** \n \n @param x the x value*/"
-- Right "@param x the x value"

-- TODO: slightly broken
commentP :: Parse String
commentP = wsP (oneLineCommentP <|> multiLineCommentP)
  where
    oneLineCommentP :: Parse String
    oneLineCommentP = wsP (P.string "//") *> many (P.satisfy (/= '/'))
    multiLineCommentP :: Parse String
    multiLineCommentP = wsP (P.string "/**") *> many middleStatements <* wsP (P.string "*/") where
      middleStatements = -- strip any leading * if they're the first character (before any characters or \n) or if they're preceded by a \n
        P.satisfy (/= '*') <|> wsP (P.satisfy (== '*') *> wsP (P.satisfy (== '/')) *> wsP (P.satisfy (== '\n')))

-- JavaDocComment parsers
-- >>> P.parse classP "private class Foo {}"
-- Right (Class (Name "Foo") (Description "{}") [])
classP = Class <$> name <*> body <*> many tagP
  where
    tagString = "class"
    -- name is the first word after the tag
    name = Name <$> wrapperP
    -- TODO: currently description is body, but that's not true
    body = Description <$> wsP (many (P.satisfy (/= '\n')))  
    wrapperP = -- skips over any characters not in 'class' string until arriving at class
      wsP (stringsP ["public", "private"]) *> wsP (P.string tagString) *> wsP (many (P.satisfy Char.isAlphaNum))
  
-- TODO: Public vs private classes?
test_classP :: Test
test_classP = TestList [
    P.parse classP "class Foo {}" ~?= Right (Class (Name "Foo") (Description "") []),
    P.parse classP "public class Foo {}" ~?= Right (Class (Name "Foo") (Description "") []),
    P.parse classP "private class Foo { }" ~?= Right (Class (Name "Foo") (Description "") []),
    P.parse classP "class Foo { /* blah */ }" ~?= Right (Class (Name "Foo") (Description "") []),
    P.parse classP "class Foo { \n // blah \n }" ~?= Right (Class (Name "Foo") (Description "") []),
    P.parse classP "class Foo { \n // blah \n } \n" ~?= Right (Class (Name "Foo") (Description "") []),
    P.parse classP "/** The Foo class \n * @version 1.0 \n */ \n class Foo { \n // blah \n } \n" ~?= Right (Class (Name "Foo") (Description "The Foo class") [Version (Description "1.0")]),
    P.parse classP "/** The Foo class \n * @version 1.0 \n * @param x the x value \n */ \n class Foo { \n // blah \n } \n" ~?= Right (Class (Name "Foo") (Description "The Foo class") [Version (Description "1.0"), Param (Name "x") (Description "the x value")])
  ]

interfaceMethodP :: Parse JavaDocComment
interfaceMethodP = undefined

classMethodP :: Parse JavaDocComment
classMethodP = undefined

methodP :: Parse JavaDocComment
methodP = wsP (interfaceMethodP <|> classMethodP)

test_methodP :: Test
test_methodP = TestList [
    P.parse interfaceMethodP "interface Foo { void bar(); }" ~?= Right (Method (Name "bar") (Description "") []),
    P.parse interfaceMethodP "class Foo { public void bar(); }" ~?= Right (Method (Name "bar") (Description "") []),
    P.parse interfaceMethodP "interface Foo { private void bar(); }" ~?= Right (Method (Name "bar") (Description "") []),
    P.parse interfaceMethodP "interface Foo { void bar(); /* blah */ }" ~?= Right (Method (Name "bar") (Description "") []),
    P.parse interfaceMethodP "class Foo { void bar(); \n // blah \n }" ~?= Right (Method (Name "bar") (Description "") []),
    P.parse interfaceMethodP "interface Foo { void bar(); \n // blah \n } \n" ~?= Right (Method (Name "bar") (Description "") []),
    P.parse interfaceMethodP "class Foo { /** The bar method \n * @version 1.0 \n */ \n void bar(); \n // blah \n } \n" ~?= Right (Method (Name "bar") (Description "The bar method") [Version (Description "1.0")]),
    P.parse interfaceMethodP "interface Foo { /** The bar method \n * @version 1.0 \n * @param x the x value \n */ \n void bar(); \n // blah \n } \n" ~?= Right (Method (Name "bar") (Description "The bar method") [Version (Description "1.0"), Param (Name "x") (Description "the x value")])
  ]

interfaceP :: Parse JavaDocComment
interfaceP = undefined

test_interfaceP :: Test
test_interfaceP = TestList [
    P.parse interfaceP "interface Foo {}" ~?= Right (Interface (Name "Foo") (Description "") []),
    P.parse interfaceP "public interface Foo {}" ~?= Right (Interface (Name "Foo") (Description "") []),
    P.parse interfaceP "private interface Foo { }" ~?= Right (Interface (Name "Foo") (Description "") []),
    P.parse interfaceP "interface Foo { /* blah */ }" ~?= Right (Interface (Name "Foo") (Description "") []),
    P.parse interfaceP "interface Foo { \n // blah \n }" ~?= Right (Interface (Name "Foo") (Description "") []),
    P.parse interfaceP "interface Foo { \n // blah \n } \n" ~?= Right (Interface (Name "Foo") (Description "") []),
    P.parse interfaceP "/** The Foo interface \n * @version 1.0 \n */ \n interface Foo { \n // blah \n } \n" ~?= Right (Interface (Name "Foo") (Description "The Foo interface") [Version (Description "1.0")]),
    P.parse interfaceP "/** The Foo interface \n * @version 1.0 \n * @param x the x value \n */ \n interface Foo { \n // blah \n } \n" ~?= Right (Interface (Name "Foo") (Description "The Foo interface") [Version (Description "1.0"), Param (Name "x") (Description "the x value")])
  ]

enumP :: Parse JavaDocComment
enumP = undefined

test_enumP :: Test
test_enumP = TestList [
    P.parse enumP "enum Foo {}" ~?= Right (Enum (Name "Foo") (Description "")),
    P.parse enumP "public enum Foo {}" ~?= Right (Enum (Name "Foo") (Description "")),
    P.parse enumP "private enum Foo { }" ~?= Right (Enum (Name "Foo") (Description "")),
    P.parse enumP "enum Foo { /* blah */ }" ~?= Right (Enum (Name "Foo") (Description "")),
    P.parse enumP "enum Foo { \n // blah \n }" ~?= Right (Enum (Name "Foo") (Description "")),
    P.parse enumP "enum Foo { \n // blah \n } \n" ~?= Right (Enum (Name "Foo") (Description ""))
  ]

javaDocCommentP :: Parse JavaDocComment
javaDocCommentP = wsP (classP <|> interfaceP <|> enumP <|> methodP)

-- TODO: add test cases
test_javaDocCommentP :: Test
test_javaDocCommentP =
  TestList []

-- JavaDoc parsers
javaDocP :: Parse JavaDoc
javaDocP = undefined
