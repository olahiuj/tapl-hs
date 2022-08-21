let rec add: Nat -> Nat -> Nat =
  fun lhs: Nat (
    if isZero lhs then
      fun rhs: Nat (
        rhs
      )
    else 
      fun rhs: Nat (
        (add (prd lhs)) (suc rhs)
      )
  )
in

let rec sum: Nat -> Nat = 
  fun n: Nat (
    if isZero n then
      Zero
    else
      (add n) (sum (prd n))
  )
in

let rec fib: Nat -> Nat =
  fun n: Nat (
    if isZero n then
      (suc Zero)
    else if isZero (prd n) then
      (suc Zero)
    else 
      (add (fib (prd n))) (fib (prd (prd n)))
  )
in
(* This is a comment block.
   (* Nested comments *)
*)

let four = (suc (suc (suc (suc Zero)))) in

let five = (add (suc Zero)) four in

fib ((add four) five)

(* fib (suc (suc (suc (suc (suc Zero))))) *)
