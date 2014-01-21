module Pretty where

import Text.PrettyPrint (Doc, (<>), (<+>))
import qualified Text.PrettyPrint as PP

parensIf ::  Bool -> Doc -> Doc
parensIf True = PP.parens
parensIf False = id

block :: [Doc] -> Doc
block [] = PP.text "{}"
block ts = (PP.text "{") PP.$$
    (PP.nest 2 ((PP.vcat $ PP.punctuate (PP.text " ; ") ts) PP.$$ PP.text "}"))

class Pretty p where
  ppr :: Int -> p -> Doc
