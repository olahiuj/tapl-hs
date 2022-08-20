module Term where
-- Runtime representations:
--  1. No type information
--  2. All variables have different names
--  3. Constructors must end with underscore "_" 

infixr :->
data Type
  = UnitType_
  | BoolType_
  | NatType_
  | Type :-> Type
  deriving ( Eq )
  ;

instance Show Type where
  show UnitType_ = "Unit"
  show NatType_  = "Nat"
  show BoolType_ = "Bool"
  show (t@(_ :-> _) :-> r) = "(" ++ show t ++ ") -> " ++ show r
  show (t :-> r) = show t ++ " -> " ++ show r

type Var = String

data Term
  = Var_ { getVar  :: Var }
  | App_ { getFun  :: Term
         , getArg  :: Term }
  | Abs_ { getPar  :: Var
         , getType :: Type
         , getBody :: Term }
  | Ite_ Term Term Term
  | Lin_ String Term Term
  | Suc_ Term
  | Prd_ Term
  | IsZ_ Term
  | Zero_
  | True_
  | False_
  | Unit_
  | Seq_ Term Term
  | Asc_ Term Type
  ;

instance Show Term where
  show (Var_ v) = v
  show (App_ m n) = "(" ++ show m ++ ") (" ++ show n ++ ")"
  show (Abs_ m t n) 
    = "func " ++ show m ++ ": " ++ show t ++ " (" ++ show n ++ ")"
  show (Suc_ m) = "suc (" ++ show m ++ ")"
  show (Prd_ m) = "prd (" ++ show m ++ ")"
  show (IsZ_ m) = "isZero (" ++ show m ++ ")"
  show Zero_ = "Zero"
  show True_ = "True"
  show False_ = "False"
  show (Ite_ t f e)
    =  "if " ++ show e
    ++ " then " ++ show t
    ++ " else " ++ show f
  show Unit_ = "unit"
  show (Seq_ m n) = show m ++ "; " ++ show n
  show (Lin_ v m n)
    =  "let " ++ show v ++ "=" ++ show m 
    ++ " in " ++ show n
  show (Asc_ tm tp) = show tm ++ " as " ++ show tp

isValue :: Term -> Bool
isValue Zero_  = True
isValue True_  = True
isValue False_ = True
isValue (Suc_ m) = isValue m
isValue (Prd_ m) = isValue m
isValue (IsZ_ m) = isValue m
isValue Abs_ {} = True
isValue Unit_  = True
isValue _ = False

sub :: String -> Term -> Term -> Term
sub x l = go where
  go (Var_ v)
    | v == x     = l
    | otherwise  = Var_ v
  go (App_ m n)  = App_ (go m) (go n)
  go (Abs_ m t n)  = Abs_ m t $ go n
  go (Suc_ m)    = Suc_ $ go m
  go (Prd_ m)    = Prd_ $ go m
  go (IsZ_ m) = IsZ_ $ go m
  go Zero_  = Zero_
  go True_  = True_
  go False_ = False_
  go (Ite_ t f e)
    = Ite_ (go t) (go f) (go e)
  go Unit_ = Unit_
  go (Seq_ m n) = Seq_ (go m) (go n)
  go (Lin_ v m n) = Lin_ v (go m) (go n)
  go (Asc_ tm tp) = Asc_ (go tm) tp

