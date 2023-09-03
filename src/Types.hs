module Types where


data Type = TNamed String
          | TArrow Type Type
  deriving (Eq, Show)
