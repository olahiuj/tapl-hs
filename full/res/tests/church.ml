let add1 = fun num: Nat (
  suc num
) in

let zero = fun f: Nat -> Nat (
  fun x: Nat (
    x
  )
) in 

let succ = fun n: 
  (Nat -> Nat) -> Nat -> Nat (
    fun f: Nat -> Nat (
      fun x: Nat (
        f (n f) x
      )
    )
) in 

let add3 = (succ succ succ zero) add1

in add3 0
