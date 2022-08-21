let add1 = fun num: Nat (
    suc num
) in

let zero = fun f0: Nat -> Nat (
    fun x0: Nat (
        x0
    )
) in 

let succ = fun nf: 
  (Nat -> Nat) -> Nat -> Nat (
    fun ff: Nat -> Nat (
        fun xf: Nat (
            ff ((nf ff) xf)
        )
    )
) in 

let add3 = (succ (succ (succ zero))) add1

in add3 Zero
      
