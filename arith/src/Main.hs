-- underscore to represent Object Language
data Term   = True_ 
            | False_ 
            | Zero_ 
            | Succ_ Term 
            | Pred_ Term 
            | Is_Zero_ Term 
            | If_Then_Else_ Term Term Term deriving (Show, Read, Eq)

isNumerical:: Term -> Bool
isNumerical t = case t of
    Zero_   -> True
    Succ_ n -> isNumerical n
    Pred_ n -> isNumerical n
    otherwise -> False

isValue:: Term -> Bool
isValue t = case t of
    True_   -> True
    False_  -> True
    otherwise -> isNumerical t
    
smallStep:: Term -> Term
smallStep t = case t of
    If_Then_Else_ True_  t1 t2 -> t1
    If_Then_Else_ False_ t1 t2 -> t2
    If_Then_Else_ e t1 t2 -> If_Then_Else_ (smallStep e) t1 t2
    
    Is_Zero_ Zero_     -> True_
    Is_Zero_ (Succ_ n) -> False_
    Is_Zero_ n -> Is_Zero_ (smallStep n)

    Pred_ Zero_ -> Zero_
    Pred_ (Succ_ n) -> n
    Succ_ n -> Succ_ (smallStep n)
    Pred_ n -> Pred_ (smallStep n)
    t -> if isValue t then t else error "Cannot Reduce"

run:: Term -> Term
run t
    | t == t' = t
    | otherwise = run t' where t' = smallStep t

-- utils: 

assert:: String -> Bool -> String
assert _   True  = "Passed"
assert msg False = error msg

-- Test1:

result = assert "Test1 Failed" $ run prog == expected where
    prog = If_Then_Else_ (Is_Zero_ $ Pred_ $ Succ_ Zero_) Zero_ False_
    expected = Zero_