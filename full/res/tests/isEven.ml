letrec isEven: Nat -> Bool = 
  func n: Nat (
    if isZero n then
      True
    else if isZero (prd n) then
      False
    else 
      isEven (prd (prd n))
  )
in 

isEven Zero
