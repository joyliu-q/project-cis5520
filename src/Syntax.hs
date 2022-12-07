module Syntax where

newtype Name = Name String deriving (Eq, Show)
newtype Description = Description String deriving (Eq, Show) -- TODO: list of DescriptionContent
newtype Link = Link String deriving (Eq, Show)

data DescriptionContent
  = Text String
    | Placeholder String
    | Reference String Link

-- @tag
-- tags: https://www.tutorialspoint.com/java/java_documentation.htm
data Tag
  = Author Description -- @author name-text
  | Deprecated Description -- @deprecated deprecatedtex
  | Param Name Description -- @param parameter-name description
  | Return Description -- @return "Returns a number representing blah blah"
  | Throws Name Description -- @throws class-name description
  | Version Description -- @version version-text
  deriving (Eq, Show)

data JavaDocHeader = JavaDocHeader Description [Tag]
  deriving (Eq, Show)

data JavaDocComment
  = Class JavaDocHeader Name
  | Method JavaDocHeader Name
  | Interface JavaDocHeader Name
  | Enum Description Name
  deriving (Eq, Show)

newtype JavaDoc = JavaDoc [JavaDocComment] deriving (Eq, Show)
-- TODO: add metadata (e.g name of file)