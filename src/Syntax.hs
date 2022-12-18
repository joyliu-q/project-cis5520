module Syntax where

import Test.QuickCheck
import Test.QuickCheck.Gen
import Data.Char (toUpper)

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

-- Generate arbitrary JavaDocComment
instance Arbitrary Tag where
  arbitrary = do
    name <- arbitrary
    desc <- arbitrary
    oneof [return $ Author desc, return $ Deprecated desc, return $ Param name desc, return $ Return desc, return $ Throws name desc, return $ Version desc]

instance Arbitrary Name where
  arbitrary = do
    name <- listOf1 $ elements ['a' .. 'z']
    -- make Name start with a capital letter
    return $ Name (toUpper (head name) : tail name)

instance Arbitrary Description where
  arbitrary = do
    desc <- listOf1 $ elements ['a' .. 'z']
    return $ Description desc
  
instance Arbitrary JavaDocHeader where
  arbitrary = do
    desc <- arbitrary
    tags <- listOf1 arbitrary
    return $ JavaDocHeader desc tags

instance Arbitrary JavaDocComment where
  arbitrary = do
    header <- arbitrary
    name <- arbitrary
    oneof [return $ Class header name, return $ Interface header name, return $ Enum (Description "") name]

instance Arbitrary JavaDoc where
  arbitrary = do
    comments <- listOf1 arbitrary
    return $ JavaDoc comments