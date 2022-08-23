let rec add: Nat -> Nat -> Nat =
  fun lhs: Nat (
    fun rhs: Nat (
      if isZero lhs then
        rhs
      else
        (add prd lhs) suc rhs
    )
  )
in 

let rec sum: Nat -> Nat = 
  fun n: Nat (
    if isZero n then
      0
    else
      (add n) sum prd n
  )
in 

let rec fib: Nat -> Nat =
  fun n: Nat (
    if isZero n then
      1
    else if isZero prd n then
      1
    else 
      (add fib prd n) fib prd prd n
  )
in
(* This is a comment block.
   (* Nested comments *)
*)

fib (add 4) 5

(* fib (suc (suc (suc (suc (suc Zero))))) *)
