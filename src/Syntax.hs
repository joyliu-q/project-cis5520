module Syntax where

newtype Name = Name String deriving (Eq, Show)
newtype Description = Description String deriving (Eq, Show)

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

data JavaDocComment
  = Class Description [Tag] Name
  | Method Description [Tag] Name
  | Interface Description [Tag] Name
  | Enum Description Name
  deriving (Eq, Show)

newtype JavaDoc = JavaDoc [JavaDocComment] deriving (Eq, Show)