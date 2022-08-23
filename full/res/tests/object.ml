let counterClass = 
  fun r: { cnt: Ref Nat } ( 
    { get = fun _: Unit !r.cnt
    , inc = fun _: Unit r.cnt := suc !r.cnt
    }
  )
in 

let newCounter =
  fun _: Unit (
    let r = {
      cnt = ref 0
    } in counterClass r
  )
in 

let resetCounterClass = 
  fun r: { cnt: Ref Nat } (
    let super = counterClass r in
      { get = super.get
      , inc = super.inc
      , reset = fun _: Unit (
        r.cnt := 0
      )}
  )
in

let newResetCounter =
  fun _: Unit (
    let r = {
      cnt = ref 0
    } in resetCounterClass r
  )
in 

let rc = newResetCounter unit
in 

(rc.inc unit);
(rc.inc unit);
(rc.inc unit);
(rc.inc unit);
(rc.reset unit);
(rc.inc unit);
(rc.get unit)
