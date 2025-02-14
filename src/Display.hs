module Display
  ( Element (..),
    Markdown (..),
    generateMarkdownObj,
    generateMarkdownText,
  )
where

import Syntax (Description (..), JavaDoc (JavaDoc), JavaDocComment (..), JavaDocHeader (..), Name (..), Tag (..))
import Test.HUnit

newtype Text = Text String deriving (Eq, Show)

-- | data type representing markdown elements
data Element
  = H1 Text
  | H2 Text
  | H3 Text
  | H4 Text
  | PlainText Text
  | Code Text
  deriving (Eq, Show)

newtype Markdown = Markdown [Element] deriving (Eq, Show)

-- | convert JavaDoc type representation in a Markdown representation
generateMarkdownObj :: JavaDoc -> Markdown
generateMarkdownObj (JavaDoc xs) = Markdown $ foldr (\x acc -> docCommentToElements x ++ acc) [] xs
  where
    docCommentToElements :: JavaDocComment -> [Element]
    docCommentToElements c = case c of
      Class header (Name name) -> H1 (Text $ "Class - " ++ name ++ "\n") : headerToMarkdown header
      Method header (Name name) -> H1 (Text $ "Method - " ++ name) : headerToMarkdown header
      Interface header (Name name) -> H1 (Text $ "Interface - " ++ name) : headerToMarkdown header
      Enum (Description "") (Name name) -> [H1 (Text $ "Enum - " ++ name)]
      Enum (Description desc) (Name name) -> [H1 (Text $ "Enum - " ++ name), H2 (Text "Description"), PlainText (Text $ desc ++ "\n")]
      Field (Description "") (Name name) -> []
      Field (Description desc) (Name name) -> []
    headerToMarkdown :: JavaDocHeader -> [Element]
    headerToMarkdown (JavaDocHeader (Description "") tagList) = tagListToMarkdown tagList
    headerToMarkdown (JavaDocHeader (Description desc) tagList) = H2 (Text "Description") : PlainText (Text $ desc ++ "\n") : tagListToMarkdown tagList
    tagListToMarkdown :: [Tag] -> [Element]
    tagListToMarkdown = foldr (\x acc -> tagToElements x ++ acc) []
    tagToElements :: Tag -> [Element]
    tagToElements tag = case tag of
      Author (Description desc) -> [H2 (Text "Author"), PlainText (Text desc)]
      Deprecated (Description desc) -> [H2 (Text "Deprecated"), PlainText (Text desc)]
      Param (Name name) (Description desc) -> [H2 (Text "Parameter"), Code (Text name), PlainText (Text $ " " ++ desc)]
      Return (Description desc) -> [H2 (Text "Return"), PlainText (Text desc)]
      Throws (Name name) (Description desc) -> [H2 (Text ("Throws - " ++ name)), PlainText (Text desc)]
      Version (Description desc) -> [H2 (Text "Version"), PlainText (Text desc)]

-- | convert Markdown representation into a Markdown string
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
      Code (Text text) -> "`" ++ text ++ "`"