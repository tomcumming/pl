module Kind where

data Kind
  = Arrow Kind Kind
  | Star
  deriving (Show, Eq)

fnKind = Arrow Star (Arrow Star (Arrow Star Star))
