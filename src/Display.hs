module Display where

import Syntax (Description (..), JavaDoc (JavaDoc), JavaDocComment (..), JavaDocHeader (..), Name (..), Tag (..))
import Test.HUnit

newtype Text = Text String deriving (Eq, Show)

data Element
  = H1 Text
  | H2 Text
  | H3 Text
  | H4 Text
  | PlainText Text
  deriving (Eq, Show)

newtype Markdown = Markdown [Element] deriving (Eq, Show)

generateMarkdownObj :: JavaDoc -> Markdown
generateMarkdownObj (JavaDoc xs) = Markdown $ foldr (\x acc -> docCommentToElements x ++ acc) [] xs
  where
    docCommentToElements :: JavaDocComment -> [Element]
    docCommentToElements c = case c of
      Class header (Name name) -> H1 (Text $ "Class - " ++ name ++ "\n") : headerToMarkdown header
      Method header (Name name) -> H1 (Text $ "Method - " ++ name) : headerToMarkdown header
      Interface header (Name name) -> H1 (Text $ "Interface - " ++ name) : headerToMarkdown header
      Enum (Description "") (Name name) -> [H1 (Text $ "Enum - " ++ name)]
      Enum (Description desc) (Name name) -> [H1 (Text $ "Enum - " ++ name), H2 (Text "Description"), PlainText (Text desc)]
      Field (Description "") (Name name) -> []
      Field (Description desc) (Name name) -> []
    headerToMarkdown :: JavaDocHeader -> [Element]
    headerToMarkdown (JavaDocHeader (Description "") tagList) = tagListToMarkdown tagList
    headerToMarkdown (JavaDocHeader (Description desc) tagList) = H2 (Text "Description") : PlainText (Text desc) : tagListToMarkdown tagList
    tagListToMarkdown :: [Tag] -> [Element]
    tagListToMarkdown = foldr (\x acc -> tagToElements x ++ acc) []
    tagToElements :: Tag -> [Element]
    tagToElements tag = case tag of
      Author (Description desc) -> [H2 (Text "Author"), PlainText (Text desc)]
      Deprecated (Description desc) -> [H2 (Text "Deprecated"), PlainText (Text desc)]
      Param (Name name) (Description desc) -> [H2 (Text ("Parameter - " ++ name)), PlainText (Text desc)]
      Return (Description desc) -> [H2 (Text "Return"), PlainText (Text desc)]
      Throws (Name name) (Description desc) -> [H2 (Text ("Throws - " ++ name)), PlainText (Text desc)]
      Version (Description desc) -> [H2 (Text "Version"), PlainText (Text desc)]

testConvertClass :: Test
testConvertClass =
  TestList
    [ generateMarkdownObj (JavaDoc [Class (JavaDocHeader (Description "This is the ArrayList class.") []) (Name "ArrayList")])
        ~?= Markdown [H1 (Text "Class - ArrayList"), H2 (Text "Description"), PlainText (Text "This is the ArrayList class.")],
      generateMarkdownObj (JavaDoc [Class (JavaDocHeader (Description "") []) (Name "Serializer")])
        ~?= Markdown [H1 (Text "Class - Serializer")],
      generateMarkdownObj (JavaDoc [Class (JavaDocHeader (Description "This is the Consumer class.") [Author (Description "John Smith")]) (Name "Consumer")])
        ~?= Markdown [H1 (Text "Class - Consumer"), H2 (Text "Description"), PlainText (Text "This is the Consumer class."), H2 (Text "Author"), PlainText (Text "John Smith")],
      generateMarkdownObj (JavaDoc [Class (JavaDocHeader (Description "This is the VersionClass class.") [Version (Description "1.0.5")]) (Name "VersionClass")])
        ~?= Markdown [H1 (Text "Class - VersionClass"), H2 (Text "Description"), PlainText (Text "This is the VersionClass class."), H2 (Text "Version"), PlainText (Text "1.0.5")]
    ]

-- >>> runTestTT testConvertClass
-- Counts {cases = 4, tried = 4, errors = 0, failures = 0}

testConvertMethod :: Test
testConvertMethod =
  TestList
    [ generateMarkdownObj (JavaDoc [Method (JavaDocHeader (Description "This is the run method.") []) (Name "run")])
        ~?= Markdown [H1 (Text "Method - run\n"), H2 (Text "Description"), PlainText (Text "This is the run method.")],
      generateMarkdownObj (JavaDoc [Method (JavaDocHeader (Description "") []) (Name "run2")])
        ~?= Markdown [H1 (Text "Method - run2\n")],
      generateMarkdownObj (JavaDoc [Method (JavaDocHeader (Description "This is the run method.") [Deprecated (Description "Support for this was removed.")]) (Name "run3")])
        ~?= Markdown [H1 (Text "Method - run3\n"), H2 (Text "Description"), PlainText (Text "This is the run method."), H2 (Text "Deprecated"), PlainText (Text "Support for this was removed.")],
      generateMarkdownObj (JavaDoc [Method (JavaDocHeader (Description "This is the run method.") [Param (Name "param1") (Description "param1 description"), Param (Name "param2") (Description "param2 description"), Return (Description "return value description"), Deprecated (Description "Support for this was removed.")]) (Name "run4")])
        ~?= Markdown [H1 (Text "Method - run4\n"), H2 (Text "Description"), PlainText (Text "This is the run method."), H2 (Text "Parameter - param1"), PlainText (Text "param1 description"), H2 (Text "Parameter - param2"), PlainText (Text "param2 description"), H2 (Text "Return"), PlainText (Text "return value description"), H2 (Text "Deprecated"), PlainText (Text "Support for this was removed.")]
    ]

-- >>> runTestTT testConvertMethod
-- Counts {cases = 4, tried = 4, errors = 0, failures = 0}

testConvertInterface :: Test
testConvertInterface =
  TestList
    [ generateMarkdownObj (JavaDoc [Interface (JavaDocHeader (Description "This is the List interface.") []) (Name "List")])
        ~?= Markdown [H1 (Text "Interface - List"), H2 (Text "Description"), PlainText (Text "This is the List interface.")],
      generateMarkdownObj (JavaDoc [Interface (JavaDocHeader (Description "") []) (Name "Serializer")])
        ~?= Markdown [H1 (Text "Interface - Serializer")],
      generateMarkdownObj (JavaDoc [Interface (JavaDocHeader (Description "This is the Consumer interface.") [Author (Description "John Smith")]) (Name "Consumer")])
        ~?= Markdown [H1 (Text "Interface - Consumer"), H2 (Text "Description"), PlainText (Text "This is the Consumer interface."), H2 (Text "Author"), PlainText (Text "John Smith")],
      generateMarkdownObj (JavaDoc [Interface (JavaDocHeader (Description "This is the VersionInterface interface.") [Version (Description "1.0.5")]) (Name "VersionInterface")])
        ~?= Markdown [H1 (Text "Interface - VersionInterface"), H2 (Text "Description"), PlainText (Text "This is the VersionInterface interface."), H2 (Text "Version"), PlainText (Text "1.0.5")]
    ]

testConvertEnum :: Test
testConvertEnum =
  TestList
    [ generateMarkdownObj (JavaDoc [Enum (Description "This is the Color enum.") (Name "Color")])
        ~?= Markdown [H1 (Text "Enum - Color"), H2 (Text "Description"), PlainText (Text "This is the Color enum.")],
      generateMarkdownObj (JavaDoc [Enum (Description "") (Name "Status")])
        ~?= Markdown [H1 (Text "Enum - Status")]
    ]

runAllGenerateMarkdownObjList :: IO Counts
runAllGenerateMarkdownObjList = runTestTT $ TestList [testConvertClass, testConvertMethod, testConvertInterface, testConvertEnum]

-- >>> runAllGenerateMarkdownObjList
-- Counts {cases = 14, tried = 14, errors = 0, failures = 0}

generateMarkdownText :: Markdown -> String
generateMarkdownText (Markdown xs) = foldr (\x acc -> go x ++ acc) [] xs
  where
    go :: Element -> String
    go elem = case elem of
      H1 (Text text) -> "# " ++ text ++ "\n"
      H2 (Text text) -> "## " ++ text ++ "\n"
      H3 (Text text) -> "### " ++ text ++ "\n"
      H4 (Text text) -> "#### " ++ text ++ "\n"
      PlainText (Text text) -> text ++ "\n"

testH1 :: Test
testH1 =
  TestList
    [ generateMarkdownText (Markdown [H1 (Text "Test Text")])
        ~?= "# Test Text\n",
      generateMarkdownText (Markdown [H1 (Text "Test Text"), H1 (Text "Test Text 2")])
        ~?= "# Test Text\n# Test Text 2\n"
    ]

testH2 :: Test
testH2 =
  TestList
    [ generateMarkdownText (Markdown [H2 (Text "Test Text")])
        ~?= "## Test Text\n",
      generateMarkdownText (Markdown [H2 (Text "Test Text"), H2 (Text "Test Text 2")])
        ~?= "## Test Text\n## Test Text 2\n"
    ]

testH3 :: Test
testH3 =
  TestList
    [ generateMarkdownText (Markdown [H3 (Text "Test Text")])
        ~?= "### Test Text\n",
      generateMarkdownText (Markdown [H3 (Text "Test Text"), H3 (Text "Test Text 2")])
        ~?= "### Test Text\n### Test Text 2\n"
    ]

testH4 :: Test
testH4 =
  TestList
    [ generateMarkdownText (Markdown [H4 (Text "Test Text")])
        ~?= "#### Test Text\n",
      generateMarkdownText (Markdown [H4 (Text "Test Text"), H4 (Text "Test Text 2")])
        ~?= "#### Test Text\n#### Test Text 2\n"
    ]

testPlainText :: Test
testPlainText =
  TestList
    [ generateMarkdownText (Markdown [PlainText (Text "Test Text")])
        ~?= "Test Text\n",
      generateMarkdownText (Markdown [PlainText (Text "Test Text"), PlainText (Text "Test Text 2")])
        ~?= "Test Text\nTest Text 2\n"
    ]

runAllGenerateMarkdownText :: IO Counts
runAllGenerateMarkdownText = runTestTT $ TestList [testH1, testH2, testH3, testH4, testPlainText]

-- >>> runAllGenerateMarkdownText
-- Counts {cases = 10, tried = 10, errors = 0, failures = 0}

{-
Example:

# Class ArrayList

## Description

desc

## tag1

explanation

# Method Name

## Desc

## Tag1

## Tag2

-}
