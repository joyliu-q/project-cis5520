{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
module DocParse where

import Control.Applicative
import Data.Char qualified as Char
import Syntax
import Parse (Parse)
import Parse qualified as P
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck qualified as QC
import qualified Data.Functor

stopAtString prefix = stopAtStringRec "" where
  stopAtStringRec :: String -> String -> String
  stopAtStringRec resAcc remainingStr = 
    case remainingStr of
      [] -> resAcc
      _ -> do
        if take (length prefix) remainingStr == prefix then resAcc
        else stopAtStringRec (resAcc ++ [head remainingStr]) (tail remainingStr)

-- Helper functions
-- takes a parser, runs it, then skips over any whitespace characters occurring afterwards
wsP :: Parse a -> Parse a
wsP p = p <* many (P.satisfy Char.isSpace)

-- >>> P.parse (many (nlP P.alpha)) "\nhiiii \n"
-- Right "hiiii"
nlP :: Parse a -> Parse a
nlP p = many (P.satisfy  (== '\n')) *> p <* many (P.satisfy  (== '\n'))

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

-- takes a parser, runs it, then returns any 'string' occurring afterwards
stringsP :: [String] -> Parse [Char]
stringsP l = case l of
  [] -> pure []
  hd : tl -> P.string hd <|> stringsP tl

-- takes a parser, runs it, and returns anything that's not a 'string' given

testStringsP :: Test
testStringsP =
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

-- Given a string "tag", returns a parser for the string that comes directly after the @tag
-- >>> P.parse (descriptionTagP "author") "@author John Smith"
-- Right (Description "John Smith")
descriptionTagP :: String -> Parse Description
descriptionTagP tag  = Description <$> (wsP (P.string tagString) *> wsP (many (P.satisfy (/= '\n'))))
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

-- >>> P.parse (commentP (many anyChar)) "// hi param x the x value\n sup"
-- Right "hi param x the x value"

-- >>> P.parse (commentP (JavaDocHeader <$> (Description <$> many (P.satisfy (/= '@'))) <*> (many tagP))) "/** \n Hi @param x the x value\n*/ public class Foo {}"
-- Right (JavaDocHeader (Description "Hi ") [Param (Name "x") (Description "the x value")])


-- >>> P.parse (oneLineCommentP (many anyChar)) "// hi param x the x value\n sup"
-- Right "hi param x the x value"
oneLineCommentP :: Parse a -> Parse a
oneLineCommentP p =  (wsP (P.string "//") *> many (P.satisfy (/= '\n'))) >>= -- apply result into P.doParse p
  \s -> case P.doParse p s of
    Nothing -> pure (error "No parses")
    Just x0 -> do
      let (x, _) = x0
      pure x

-- >>> P.parse (multiLineCommentP (many anyChar)) "/**\n* The Foo class \n*/hi"
-- Right "The Foo class \n"

-- >>> P.parse ((multiLineCommentP (many anyChar)) *> many anyChar) "/**\n* Hi\n* @param x the x value\n*/ hi hi public class Foo {}"
-- Right "hi hi public class Foo {}"

multiLineCommentP :: Parse a -> Parse a
multiLineCommentP p = wsP (P.string "/**") *> wsP P.endCommentP >>=
  \s -> 
    case P.doParse (wsP (P.string "*") *> p) s of
      Nothing -> pure (error "No parses")
      Just x0 -> do
        let (x, str) = x0
        pure x
  <* wsP (stringsP ["*/"])


commentP :: Parse a -> Parse a
commentP p =
  nlP (wsP (oneLineCommentP p <|> multiLineCommentP p))

-- >>> P.parse classP "public class Foo {}"
-- Right (Class (JavaDocHeader (Description "") []) (Name "Foo"))

-- >>> P.parse classP "/**\n* The Foo class \n*/\nclass Foo { \n // blah \n } \n"
-- Right (Class (JavaDocHeader (Description "The Foo class \n") []) (Name "Foo"))

-- >>> P.parse classP "/**\n* The Foo class \n* @param x the x value\n*/\npublic class Foo {}"
-- Right (Class (JavaDocHeader (Description "The Foo class \n") [Param (Name "x") (Description "the x value")]) (Name "Foo"))

classP = Class <$> header <*> name
  where
    -- comment is any string that is not a tagP
    descriptionP = Description <$> many (P.satisfy (/= '*'))
    tags = wsP (many ((P.string "*" *> tagP) <|> tagP))
    header = commentP (JavaDocHeader <$> descriptionP <*> tags) <|> (JavaDocHeader (Description "") <$> tags)

    -- description = Description <$> nlP (commentP (many (P.satisfy (/= '@'))))
    name = -- skips over any characters not in 'class' string until arriving at class
      Name <$> (wsP (stringsP ["public", "private", "protected"]) *> wsP (P.string "class") *> wsP (many (P.satisfy Char.isAlphaNum)))

-- Public vs private classes?
test_classP :: Test
test_classP = TestList [
    P.parse classP "class Foo {}" ~?= Right (Class (JavaDocHeader (Description "") []) (Name "Foo")),
    P.parse classP "public class Foo {}" ~?= Right (Class (JavaDocHeader (Description "") []) (Name "Foo")),
    P.parse classP "private class Foo { }" ~?= Right (Class (JavaDocHeader (Description "") []) (Name "Foo")),
    P.parse classP "class Foo { /* blah */ }" ~?= Right (Class (JavaDocHeader (Description "") []) (Name "Foo")),
    P.parse classP "class Foo { \n // blah \n }" ~?= Right (Class (JavaDocHeader (Description "") []) (Name "Foo")),
    P.parse classP "class Foo { \n // blah \n } \n" ~?= Right (Class (JavaDocHeader (Description "") []) (Name "Foo")),
    P.parse classP "/**\n* The Foo class\n*/\nclass Foo { \n // blah \n } \n" ~?= Right (Class (JavaDocHeader (Description "The Foo class\n") []) (Name "Foo")),
    P.parse classP "/**\n* The Foo class\n* @version 1.0\n */ \n class Foo { \n // blah \n } \n" ~?= Right (Class (JavaDocHeader (Description "The Foo class\n") [Version (Description "1.0")]) (Name "Foo")),
    P.parse classP "/**\n* The Foo class\n* @version 1.0\n* @param x the x value\n*/ \n class Foo { \n // blah \n } \n" ~?= Right (Class (JavaDocHeader (Description "The Foo class\n") [Version (Description "1.0"), Param (Name "x") (Description "the x value")]) (Name "Foo"))
  ]

interfaceMethodP :: Parse JavaDocComment
interfaceMethodP = Method <$> header <*> name
  where
    descriptionP = Description <$> many (P.satisfy (/= '*'))
    tags = wsP (many ((P.string "*" *> tagP) <|> tagP))
    header = commentP (JavaDocHeader <$> descriptionP <*> tags) <|> (JavaDocHeader (Description "") <$> tags)
    name = Name <$> (wsP (stringsP ["public", "private", "protected"]) *> wsP (P.string "void") *> wsP (many (P.satisfy Char.isAlphaNum)))

-- TODO: differentiate between interfaceMethod & normal method
classMethodP :: Parse JavaDocComment
classMethodP = interfaceMethodP

methodP :: Parse JavaDocComment
methodP = wsP (interfaceMethodP <|> classMethodP)

test_methodP :: Test
test_methodP = TestList [
    P.parse interfaceMethodP "void bar();" ~?= Right (Method (JavaDocHeader (Description "") []) (Name "bar")),
    P.parse interfaceMethodP "public void bar();" ~?= Right (Method (JavaDocHeader (Description "") []) (Name "bar")),
    P.parse interfaceMethodP "private void bar();" ~?= Right (Method (JavaDocHeader (Description "") []) (Name "bar")),
    P.parse interfaceMethodP "void bar(); // comment here" ~?= Right (Method (JavaDocHeader (Description "") []) (Name "bar")),
    P.parse interfaceMethodP "/**\n* The bar method\n* @version 1.0\n*/\nvoid bar();" ~?= Right (Method (JavaDocHeader (Description "The bar method\n") [Version (Description "1.0")]) (Name "bar"))
  ]

interfaceP :: Parse JavaDocComment
interfaceP = Interface <$> header <*> name
  where
    descriptionP = Description <$> many (P.satisfy (/= '*'))
    tags = wsP (many ((P.string "*" *> tagP) <|> tagP))
    header = commentP (JavaDocHeader <$> descriptionP <*> tags) <|> (JavaDocHeader (Description "") <$> tags)
    name = Name <$> (wsP (stringsP ["public", "private", "protected"]) *> wsP (P.string "interface") *> wsP (many (P.satisfy Char.isAlphaNum)))

test_interfaceP :: Test
test_interfaceP = TestList [
    P.parse interfaceP "interface Foo {}" ~?= Right (Interface (JavaDocHeader (Description "") []) (Name "Foo")),
    P.parse interfaceP "public interface Foo {}" ~?= Right (Interface (JavaDocHeader (Description "") []) (Name "Foo")),
    P.parse interfaceP "private interface Foo { }" ~?= Right (Interface (JavaDocHeader (Description "") []) (Name "Foo")),
    P.parse interfaceP "interface Foo { /* blah */ }" ~?= Right (Interface (JavaDocHeader (Description "") []) (Name "Foo")),
    P.parse interfaceP "interface Foo { \n // blah \n }" ~?= Right (Interface (JavaDocHeader (Description "") []) (Name "Foo")),
    P.parse interfaceP "interface Foo { \n // blah \n } \n" ~?= Right (Interface (JavaDocHeader (Description "") []) (Name "Foo")),
    P.parse interfaceP "/**\n* The Foo interface\n* @version 1.0\n*/\ninterface Foo { \n // blah \n } \n" ~?= Right (Interface (JavaDocHeader (Description "The Foo interface\n") [Version (Description "1.0")]) (Name "Foo")),
    P.parse interfaceP "/**\n* The Foo interface\n* @version 1.0\n* @param x the x value\n*/\ninterface Foo { \n // blah \n } \n" ~?= Right (Interface (JavaDocHeader (Description "The Foo interface\n") [Version (Description "1.0"), Param (Name "x") (Description "the x value")]) (Name "Foo"))
  ]

enumP :: Parse JavaDocComment
enumP = undefined

test_enumP :: Test
test_enumP = TestList [
    P.parse enumP "enum Foo {}" ~?= Right (Enum (Description "") (Name "Foo")),
    P.parse enumP "public enum Foo {}" ~?= Right (Enum (Description "") (Name "Foo")),
    P.parse enumP "private enum Foo { }" ~?= Right (Enum (Description "") (Name "Foo")),
    P.parse enumP "enum Foo { /* blah */ }" ~?= Right (Enum (Description "") (Name "Foo")),
    P.parse enumP "enum Foo { \n // blah \n }" ~?= Right (Enum (Description "") (Name "Foo")),
    P.parse enumP "enum Foo { \n // blah \n } \n" ~?= Right (Enum (Description "") (Name "Foo"))
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

-- TODO: roundtrip testing to print out the parsed value
-- TODO: add param auto?
