module Display where

import Syntax (Description (..), JavaDoc (JavaDoc), JavaDocComment (..), Name (..), Tag (..))
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
generateMarkdownObj doc = undefined

testConvertClass :: Test
testConvertClass =
  TestList
    [ generateMarkdownObj (JavaDoc [Class (Description "This is the ArrayList class.") [] (Name "ArrayList")])
        ~?= Markdown [H1 (Text "Class ArrayList"), H2 (Text "Description"), PlainText (Text "This is the ArrayList class.")],
      generateMarkdownObj (JavaDoc [Class (Description "") [] (Name "Serializer")])
        ~?= Markdown [H1 (Text "Class ArrayList")],
      generateMarkdownObj (JavaDoc [Class (Description "This is the Consumer class.") [Author (Description "John Smith")] (Name "Consumer")])
        ~?= Markdown [H1 (Text "Class Consumer"), H2 (Text "Description"), PlainText (Text "This is the Consumer class."), H2 (Text "Author"), PlainText (Text "John Smith")],
      generateMarkdownObj (JavaDoc [Class (Description "This is the VersionClass class.") [Version (Description "1.0.5")] (Name "VersionClass")])
        ~?= Markdown [H1 (Text "Class VersionClass"), H2 (Text "Description"), PlainText (Text "This is the VersionClass class."), H2 (Text "Version"), PlainText (Text "1.0.5")]
    ]

testConvertMethod :: Test
testConvertMethod =
  TestList
    [ generateMarkdownObj (JavaDoc [Method (Description "This is the run method.") [] (Name "run")])
        ~?= Markdown [H1 (Text "Method run"), H2 (Text "Description"), PlainText (Text "This is the run method.")],
      generateMarkdownObj (JavaDoc [Method (Description "") [] (Name "run2")])
        ~?= Markdown [H1 (Text "Method run2")],
      generateMarkdownObj (JavaDoc [Method (Description "This is the run method.") [Deprecated (Description "Support for this was removed.")] (Name "run3")])
        ~?= Markdown [H1 (Text "Method run3"), H2 (Text "Description"), PlainText (Text "This is the run method."), H2 (Text "Deprecated"), PlainText (Text "Support for this was removed.")],
      generateMarkdownObj (JavaDoc [Method (Description "This is the run method.") [Param (Name "param1") (Description "param1 description"), Param (Name "param2") (Description "param2 description"), Return (Description "return value description"), Version (Description "1.0.5")] (Name "run4")])
        ~?= Markdown [H1 (Text "Method run4"), H2 (Text "Description"), PlainText (Text "This is the run method."), H2 (Text "Parameter param1"), PlainText (Text "Parameter param1 description"), H2 (Text "Parameter param2"), PlainText (Text "Parameter param2 description"), H2 (Text "Return"), PlainText (Text "return value description"), H2 (Text "Deprecated"), PlainText (Text "Support for this was removed.")]
    ]

testConvertInterface :: Test
testConvertInterface =
  TestList
    [ generateMarkdownObj (JavaDoc [Interface (Description "This is the List interface.") [] (Name "List")])
        ~?= Markdown [H1 (Text "Interface List"), H2 (Text "Description"), PlainText (Text "This is the List interface.")],
      generateMarkdownObj (JavaDoc [Interface (Description "") [] (Name "Serializer")])
        ~?= Markdown [H1 (Text "Interface List")],
      generateMarkdownObj (JavaDoc [Interface (Description "This is the Consumer interface.") [Author (Description "John Smith")] (Name "Consumer")])
        ~?= Markdown [H1 (Text "Interface Consumer"), H2 (Text "Description"), PlainText (Text "This is the Consumer interface."), H2 (Text "Author"), PlainText (Text "John Smith")],
      generateMarkdownObj (JavaDoc [Interface (Description "This is the VersionInterface interface.") [Version (Description "1.0.5")] (Name "VersionInterface")])
        ~?= Markdown [H1 (Text "Interface VersionInterface"), H2 (Text "Description"), PlainText (Text "This is the VersionInterface interface."), H2 (Text "Version"), PlainText (Text "1.0.5")]
    ]

testConvertEnum :: Test
testConvertEnum =
  TestList
    [ generateMarkdownObj (JavaDoc [Enum (Description "This is the Color enum.") (Name "Color")])
        ~?= Markdown [H1 (Text "Enum Color"), H2 (Text "Description"), PlainText (Text "This is the Color enum.")],
      generateMarkdownObj (JavaDoc [Enum (Description "") (Name "Status")])
        ~?= Markdown [H1 (Text "Enum Status")]
    ]

generateMarkdownText :: Markdown -> String
generateMarkdownText mkd = undefined

testH1 :: Test
testH1 =
  TestList
    [ generateMarkdownText (Markdown [H1 (Text "Test Text")])
        ~?= "# Test Text",
      generateMarkdownText (Markdown [H1 (Text "Test Text"), H1 (Text "Test Text 2")])
        ~?= "# Test Text \n # Test Text 2"
    ]

testH2 :: Test
testH2 =
  TestList
    [ generateMarkdownText (Markdown [H2 (Text "Test Text")])
        ~?= "## Test Text",
      generateMarkdownText (Markdown [H2 (Text "Test Text"), H2 (Text "Test Text 2")])
        ~?= "## Test Text \n ## Test Text 2"
    ]

testH3 :: Test
testH3 =
  TestList
    [ generateMarkdownText (Markdown [H3 (Text "Test Text")])
        ~?= "### Test Text",
      generateMarkdownText (Markdown [H3 (Text "Test Text"), H3 (Text "Test Text 2")])
        ~?= "### Test Text \n ### Test Text 2"
    ]

testH4 :: Test
testH4 =
  TestList
    [ generateMarkdownText (Markdown [H4 (Text "Test Text")])
        ~?= "#### Test Text",
      generateMarkdownText (Markdown [H4 (Text "Test Text"), H4 (Text "Test Text 2")])
        ~?= "#### Test Text \n #### Test Text 2"
    ]

testPlainText :: Test
testPlainText =
  TestList
    [ generateMarkdownText (Markdown [PlainText (Text "Test Text")])
        ~?= "Test Text",
      generateMarkdownText (Markdown [PlainText (Text "Test Text"), PlainText (Text "Test Text 2")])
        ~?= "Test Text \n Test Text 2"
    ]

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