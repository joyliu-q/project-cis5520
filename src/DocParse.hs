module DocParse where

import Control.Applicative
import Data.Char qualified as Char
import Data.Functor qualified
import Display
import Parse (Parse)
import Parse qualified as P
import Syntax
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck
import Test.QuickCheck qualified as QC
import Data.List

-- Helper functions
wsP :: Parse a -> Parse a
wsP p = p <* many (P.satisfy Char.isSpace)

nlP :: Parse a -> Parse a
nlP p = many (P.satisfy (== '\n')) *> p <* many (P.satisfy (== '\n'))

braces :: Parse String
braces = wsP (P.string "{") <* many (P.satisfy (/= '}')) <* wsP (P.string "}")

paren :: Parse String
paren = wsP (P.string "(") <* many (P.satisfy (/= ')')) <* wsP (P.string ")")

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

stringsP :: [String] -> Parse [Char]
stringsP l = case l of
  [] -> pure []
  hd : tl -> P.string hd <|> stringsP tl
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
descriptionTagP :: String -> Parse Description
descriptionTagP tag = Description <$> (wsP (P.string tagString) *> wsP (many (P.satisfy (/= '\n'))))
  where
    tagString = "@" ++ tag

-- Given a string "tag", returns a parser for the Name (first word) and Description (rest of the string) that comes directly after the @tag
nameDescriptionTagP :: String -> (Parse Name, Parse Description)
nameDescriptionTagP tag = do
  let tagString = "@" ++ tag
  let name = Name <$> (wsP (P.string tagString) *> wsP (many (P.satisfy Char.isAlphaNum)))
  let description = Description <$> wsP (many (P.satisfy (/= '\n')))
  (name, description)

tagP :: Parse Tag
tagP = wsP (authorP <|> paramP <|> returnP <|> throwsP <|> versionP <|> deprecatedP)
  where
    authorP = Author <$> descriptionTagP "author"
    paramP = Param <$> name <*> description
      where
        (name, description) = nameDescriptionTagP "param"
    returnP = Return <$> descriptionTagP "return"
    throwsP = Throws <$> name <*> description
      where
        (name, description) = nameDescriptionTagP "throws"
    versionP = Version <$> descriptionTagP "version"
    deprecatedP = Deprecated <$> descriptionTagP "deprecated"

test_tagP :: Test
test_tagP =
  TestList
    [ P.parse tagP "@param x" ~?= Right (Param (Name "x") (Description "")),
      P.parse tagP "@param x the x value" ~?= Right (Param (Name "x") (Description "the x value")),
      P.parse tagP "@param x the x value\n" ~?= Right (Param (Name "x") (Description "the x value"))
    ]

-- Comment Parsers
-- Extract inner values of a comment. Comment can either by enclosed by /* */ (multiline) or // (single line)
oneLineCommentP :: Parse a -> Parse a
oneLineCommentP p =
  (wsP (P.string "//") *> many (P.satisfy (/= '\n')))
    >>= \s -> case P.doParse p s of -- apply result into P.doParse p
      Nothing -> pure (error "No parses")
      Just x0 -> do
        let (x, _) = x0
        pure x

trimLeading :: Parse a -> Parse a
trimLeading p = wsP (many (P.char ' ') *> many (P.string "*")) *> many (wsP (P.string "*\n")) *> wsP (many (P.char ' ') *> many (P.string "*")) *> p

splitOn :: String -> Char -> [String]
splitOn s c = case dropWhile (== c) s of
  "" -> []
  s' -> 
      -- keep the delimiter
      let (w, s'') = break (== c) s' in
      w : splitOn s'' c

processCommentLine :: String -> String
processCommentLine s = 
  let sList = splitOn s '\n' in 
  let process list = case list of
        [] -> ""
        hd : tl -> 
          let trimEnd = dropWhileEnd Char.isSpace hd in
          let trimStart = dropWhile Char.isSpace trimEnd in
          (trimStart ++ ['\n']) ++ process tl
  in
  process sList

multiLineCommentP :: Parse a -> Parse a
multiLineCommentP p =
  wsP (P.string "/**") *> wsP P.endCommentP
    >>= \s ->
      let processedS = processCommentLine s in
      case P.doParse (trimLeading p) processedS of
        Nothing -> pure (error "No parses")
        Just x0 -> do
          let (x, str) = x0
          pure x
        <* wsP (stringsP ["*/"])

commentP :: Parse a -> Parse a
commentP p =
  nlP (wsP (oneLineCommentP p <|> multiLineCommentP p))

-- Class & Interface Parsers
descriptionP = Description <$> (many (P.satisfy (/= '*')) >>= 
  \s -> 
    let trimEnd = dropWhileEnd Char.isSpace s in
    let trimStart = dropWhile Char.isSpace trimEnd in
    pure trimStart)
tags = wsP (many ((wsP (P.string "*") *> tagP) <|> tagP))
classP = Class <$> header <*> name <* wsP (P.string "{")
  where
    -- comment is any string that is not a tagP
    header = commentP (JavaDocHeader <$> descriptionP <*> tags) <|> (JavaDocHeader (Description "") <$> tags)
    name =
      -- skips over any characters not in 'class' string until arriving at class
      Name <$> (wsP (stringsP ["public", "private", "protected"]) *> wsP (P.string "class") *> wsP (many (P.satisfy Char.isAlphaNum)))

test_classP :: Test
test_classP =
  TestList
    [ P.parse classP "class Foo {}" ~?= Right (Class (JavaDocHeader (Description "") []) (Name "Foo")),
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
interfaceMethodP = Method <$> header <*> name <* paren <* many (P.satisfy (/= ';')) <* wsP (P.string ";")
  where
    header = commentP (JavaDocHeader <$> descriptionP <*> tags) <|> (JavaDocHeader (Description "") <$> tags)
    name = Name <$> (wsP (stringsP ["public", "private", "protected"]) *> wsP (many P.alpha) *> wsP (many (P.satisfy Char.isAlphaNum)))

classMethodP :: Parse JavaDocComment
classMethodP = Method <$> header <*> name <* paren <* many (P.satisfy (/= '{')) <* braces
  where
    tags =
      wsP (many ((P.string "*" *> tagP) <|> tagP))
    header = commentP (JavaDocHeader <$> descriptionP <*> tags) <|> (JavaDocHeader (Description "") <$> tags)
    name = Name <$> (wsP (stringsP ["public", "private", "protected"]) *> wsP (many P.alpha) *> wsP (many (P.satisfy Char.isAlphaNum)))

methodP :: Parse JavaDocComment
methodP = wsP (interfaceMethodP <|> classMethodP)

test_methodP :: Test
test_methodP =
  TestList
    [ P.parse methodP "void bar();" ~?= Right (Method (JavaDocHeader (Description "") []) (Name "bar")),
      P.parse methodP "public void bar(){}" ~?= Right (Method (JavaDocHeader (Description "") []) (Name "bar")),
      P.parse methodP "private void bar();" ~?= Right (Method (JavaDocHeader (Description "") []) (Name "bar")),
      P.parse methodP "void bar(); // comment here" ~?= Right (Method (JavaDocHeader (Description "") []) (Name "bar")),
      P.parse methodP "/**\n* The bar method\n* @version 1.0\n*/\nvoid bar();" ~?= Right (Method (JavaDocHeader (Description "The bar method\n") [Version (Description "1.0")]) (Name "bar"))
    ]

interfaceP :: Parse JavaDocComment
interfaceP = Interface <$> header <*> name <* wsP (P.string "{")
  where
    header = commentP (JavaDocHeader <$> descriptionP <*> tags) <|> (JavaDocHeader (Description "") <$> tags)
    name = 
      Name <$> (wsP (stringsP ["public", "private", "protected"]) *> wsP (P.string "interface") *> wsP (many (P.satisfy Char.isAlphaNum)))

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
      P.parse interfaceP "/**\n* The Foo interface\n* @version 1.0\n* @param x the x value\n*/\ninterface Foo { \n // blah \n } \n" ~?= Right (Interface (JavaDocHeader (Description "The Foo interface") [Version (Description "1.0"), Param (Name "x") (Description "the x value")]) (Name "Foo"))
    ]

classAndMethodP :: Parse [JavaDocComment]
classAndMethodP =
  do
    c <- classP
    ms <- many classMethodP
    res <- wsP (P.string "}")
    return (c : ms)

interfaceAndMethodP :: Parse [JavaDocComment]
interfaceAndMethodP =
  do
    c <- interfaceP
    ms <- many interfaceMethodP
    res <- wsP (P.string "}")
    return (c : ms)

test_interfaceAndMethodP :: Test
test_interfaceAndMethodP =
  TestList
    [ P.parse interfaceAndMethodP "public interface Test {\n  /**\n   * This is a description of the method.\n   *\n   * @param incomingDamage incoming damage of the attack\n   * @param damageType     type of damage being done\n   * @version 1.05\n   * @return how much damage actually landed\n   * @throws IllegalArgumentException incoming damage is negative\n   */\n  public int successfullyAttacked(int incomingDamage, String damageType) throws IllegalArgumentException;\n}" ~?= Right [Interface (JavaDocHeader (Description "") []) (Name "Test"),Method (JavaDocHeader (Description "This is a description of the method.") []) (Name "successfullyAttacked")]
    ]

-- Enum Parsers
enumP :: Parse JavaDocComment
enumP = Enum <$> header <*> name <* wsP (P.string "{") <* wsP (many (P.satisfy (/= '}'))) <* wsP (P.string "}")
  where
    header = commentP descriptionP <|> Description <$> wsP (P.string "")
    name = Name <$> (wsP (stringsP ["public", "private", "protected"]) *> wsP (P.string "enum") *> wsP (many (P.satisfy Char.isAlphaNum)))

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

-- JavaDoc Parsers
javaDocCommentP :: Parse [JavaDocComment]
javaDocCommentP = classAndMethodP <|> interfaceAndMethodP <|> commentToSingleton enumP where
  commentToSingleton p = do
  c <- p
  return [c]

javaDocCommentsP :: Parse [JavaDocComment]
javaDocCommentsP = do
  res <- many javaDocCommentP
  return (concat res)

test_javaDocCommentP :: Test
test_javaDocCommentP =
  TestList []

-- JavaDoc parsers
javaDocP :: Parse JavaDoc
javaDocP = JavaDoc <$> javaDocCommentsP

test_javaDocP :: Test
test_javaDocP =
  TestList
    [ P.parse javaDocP "public class Foo {\n/**\n* The Foo class\n* @version 1.0\n*/\npublic void bar() {\n}\n}" ~?= Right (JavaDoc [Class (JavaDocHeader (Description "") []) (Name "Foo"), Method (JavaDocHeader (Description "The Foo class\n") [Version (Description "1.0")]) (Name "bar")])
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

-- Generate arbitrary JavaDocComment
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

generateJavaDocText :: JavaDoc -> String
generateJavaDocText (JavaDoc jdc) = case jdc of
  [] -> ""
  (x : xs) -> generateJavaDocCommentText x ++ "\n\n" ++ generateJavaDocText (JavaDoc xs)

-- Quickcheck roundtrip tests
-- >>> generateJavaDocText (JavaDoc [Enum (Description "") (Name "Pobk"),Class (JavaDocHeader (Description "ic") [Throws (Name "Rx") (Description "qyo")]) (Name "Ym"),Enum (Description "") (Name "Fioh")])
-- "/**\n * */\npublic enum Pobk {\n}\n\n\n/**\n * ic\n * @throws Rx qyo\n*/\npublic class Ym {\n}\n\n\n/**\n * */\npublic enum Fioh {\n}\n\n\n"

-- >>> P.parse javaDocP "/**\n * */\npublic enum Pobk {\n}\n\n\n/**\n * ic\n * @throws Rx qyo\n*/\npublic class Ym {\n}\n\n\n/**\n * */\npublic enum Fioh {\n}\n\n\n"
-- Right (JavaDoc [Enum (Description "") (Name "Pobk"),Class (JavaDocHeader (Description "ic") [Throws (Name "Rx") (Description "qyo")]) (Name "Ym")])

-- generateJavaDocText (JavaDoc [Enum (Description "") (Name "Pobk"),Class (JavaDocHeader (Description "ic") [Throws (Name "Rx") (Description "qyo")]) (Name "Ym"),Enum (Description "") (Name "Fioh")])
-- generateJavaDocTAXT (JavaDoc [Enum (Description "") (Name "Pobk"),Class (JavaDocHeader (Description "ic") [Throws (Name "Rx") (Description "qyo")]) (Name "Ym")])


prop_roundtrip :: JavaDoc -> Bool
prop_roundtrip jd =
  let javaDocStr = generateJavaDocText jd
      parsedJavaDoc = P.parse javaDocP javaDocStr
   in case parsedJavaDoc of
        Left err -> error "unable to parse"
        Right pJD -> jd == pJD


-- Read in a file example/Foo.java as a string and run javaDocP on the string
-- >>> main
main :: IO ()
main = do
  contents <- readFile "example/Test.java"
  let parsedDoc = P.parse javaDocP contents
   in case parsedDoc of
        Left err -> error "unable to parse"
        Right pD ->
          let markdownObj = generateMarkdownObj pD
           in let markdownStr = generateMarkdownText markdownObj
               in writeFile "example/TestDoc.md" markdownStr
