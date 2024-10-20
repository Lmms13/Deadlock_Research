type ForkExchange = !Int;?();!();ForkExchange

philosopher : Int -> ForkExchange 1-> ForkExchange 1-> ()
philosopher id left right =
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is thinking.");
    let left = send id left in
    let right = send id right in
    let (_, left) = receive left in
    let (_, right) = receive right in
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
    let left = send () left in
    let right = send () right in
    philosopher id left right

fork_ : dualof ForkExchange -> dualof ForkExchange 1-> ()
fork_ left right =
    let (id,right) = receive right in
    let (_,left) = receive left in
    if(even id) 
    then
        let left = unitaryFork left in
        let right = unitaryFork right in
        fork_ left right
    else
        let right = unitaryFork right in
        let left = unitaryFork left in
        fork_ left right

unitaryFork : !();?();dualof ForkExchange -> dualof ForkExchange
unitaryFork f =
    let f = send () f in
    let (_, f) = receive f in
    f

main : ()
main =
    let (p1, f1) = new @ForkExchange () in
    let (p2, f2) = new @ForkExchange () in
    let (p3, f3) = new @ForkExchange () in
    let (p4, f4) = new @ForkExchange () in
    let (p5, f5) = new @ForkExchange () in
    let (p6, f6) = new @ForkExchange () in
    let (p7, f7) = new @ForkExchange () in
    let (p8, f8) = new @ForkExchange () in
    let (p9, f9) = new @ForkExchange () in
    let (p10, f10) = new @ForkExchange () in
    fork @() (\_ : () 1-> fork_ f1 f2);
    fork @() (\_ : () 1-> fork_ f3 f4);
    fork @() (\_ : () 1-> fork_ f5 f6);
    fork @() (\_ : () 1-> fork_ f7 f8);
    fork @() (\_ : () 1-> fork_ f9 f10);
    fork @() (\_ : () 1-> philosopher 1 p1 p10);
    fork @() (\_ : () 1-> philosopher 2 p2 p3);
    fork @() (\_ : () 1-> philosopher 3 p4 p5);
    fork @() (\_ : () 1-> philosopher 4 p6 p7);
    philosopher 5 p8 p9;
    print @String "Done!"

