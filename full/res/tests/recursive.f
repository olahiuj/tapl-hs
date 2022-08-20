letrec add: Nat -> Nat -> Nat =
  func lhs: Nat (
    if isZero lhs then
      func rhs1: Nat (
        rhs1
      )
    else 
      func rhs2: Nat (
        (add (prd lhs)) (suc rhs2)
      )
  )
in

letrec sum: Nat -> Nat = 
  func i: Nat (
    if isZero i then
      Zero
    else
      (add i) (sum (prd i))
  )
in

letrec fib: Nat -> Nat =
  func o: Nat (
    if isZero o then
      (suc Zero)
    else if isZero (prd o) then
      (suc Zero)
    else 
      (add (fib (prd o))) (fib (prd (prd o)))
  )
in

(fib (suc (suc (suc (suc (suc Zero))))))

// fib (suc (suc (suc (suc (suc Zero)))))
