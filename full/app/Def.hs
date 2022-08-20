-- Text -> Token -> FTerm -> FTerm -> RTerm -> Value
--      |        |        |        |        |
--    lexer    parser  desugar  compile   eval
module Def where

type Name = String

-- Desugared Terms
type DTerm = FTerm

-- Full Terms, i.e. static
data FTerm
  = VarF Name     
  | AppF FTerm FTerm
  | AbsF Name  FType FTerm
  | IteF FTerm FTerm FTerm
  | PrdF FTerm
  | SucF FTerm    
  | IsZF FTerm    
  | FalseF        
  | TrueF         
  | ZeroF         
  | UnitF
  | AscF FTerm FType
  | FixF FTerm
  | LriF Name  FType FTerm FTerm
  | LinF Name  FTerm FTerm
  | SeqF FTerm FTerm
  | FldF [(Name, FTerm)]
  | AccF FTerm Name
  deriving (Show, Eq)

infixr :=>
data FType
  = UnitTypeF
  | BoolTypeF
  | NatTypeF
  | FType :=> FType
  | FldTypeF [(Name, FType)]
  deriving (Show, Eq)

-- Runtime Terms
data RTerm
  = VarR Int
  | AppR RTerm RTerm
  | AbsR RTerm
  | IteR RTerm RTerm RTerm
  | SucR RTerm
  | PrdR RTerm
  | IsZR RTerm
  | ZeroR
  | TrueR
  | FalseR
  | UnitR
  | AscR RTerm RType
  | FixR RTerm
  | FldR [(Name, RTerm)]
  | AccR RTerm Name
  deriving (Eq)

infixr :->
data RType
  = UnitTypeR
  | BoolTypeR
  | NatTypeR
  | RType :-> RType
  | FldTypeR [(Name, RType)]
  deriving (Show, Eq)

instance Show RTerm where
  show n@(SucR m) = show $ count n
    where count ZeroR = 0
          count (SucR x) = 1 + count x
          count _ = error "not a Number"
  show FalseR = "False"
  show TrueR = "True"
  show UnitR = "Unit"
  show (FldR fs) = "{ " ++ foldl1 (\x y -> x ++ ", " ++ y) (showpair <$> fs) ++ " }"
    where showpair (x, y) = x ++ "= " ++ show y
  show (AccR m f) = show m ++ "." ++ f
  show _ = "not a value"
