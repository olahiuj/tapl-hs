let add1 = func num: Nat (
    suc num
) in

let zero = func f0: Nat -> Nat (
    func x0: Nat (
        x0
    )
) in 

let succ = func nf: 
  (Nat -> Nat) -> Nat -> Nat (
    func ff: Nat -> Nat (
        func xf: Nat (
            ff ((nf ff) xf)
        )
    )
) in 

let add4 = (succ (succ (succ zero))) add1

in add4 Zero
      
