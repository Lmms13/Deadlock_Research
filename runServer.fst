type SharedCounter : *S = *?Counter
type Counter : 1S = +{ Inc: Wait
                     , Dec: Wait
                     , Get: ?Int ; Wait
                     }

-- | Handler for a counter
counterService : Int -> dualof Counter 1-> Int
counterService i (Inc c) = close c ; i + 1 
counterService i (Dec c) = close c ; i - 1
counterService i (Get c) = c |> send i |> close ; i

-- | Counter server
runCounterServer : dualof SharedCounter -> Diverge
runCounterServer = runServer @Counter @Int counterService 0 

main : ()
main = 
    let c = forkWith @SharedCounter @() runCounterServer in
    forkWith @Counter @Int (counterService 0)
