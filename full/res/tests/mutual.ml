let rec r: {
  isEven: Nat -> Bool,
  isOdd : Nat -> Bool
} = {
  isEven = fun x: Nat (
    if isZero x then
      True
    else
      (r.isOdd) (prd x)
  ),
  isOdd  = fun x: Nat (
    if isZero x then
      False
    else
      (r.isEven) (prd x)
  )
}

in (r.isEven) (suc (suc Zero))

