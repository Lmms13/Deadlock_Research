type ForkExchange = !();?();!();ForkExchange

philosopher : Int -> ForkExchange 1-> ForkExchange 1-> ()
philosopher id left right =
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is thinking.");
    let left = send () left in
    let right = send () right in
    let (_, left) = receive left in
    let (_, right) = receive right in
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
    let left = send () left in
    let right = send () right in
    philosopher id left right

fork_ : dualof ForkExchange -> dualof ForkExchange 1-> ()
fork_ left right =
    let right = unitaryFork right in
    let left = unitaryFork left in
    fork_ left right

unitaryFork : dualof ForkExchange -> dualof ForkExchange
unitaryFork f =
    let (_,f) = receive f in
    let f = send () f in
    let (_, f) = receive f in
    f

recUnitaryFork : dualof ForkExchange -> ()
recUnitaryFork f =
    let (_,f) = receive f in
    let f = send () f in
    let (_, f) = receive f in
    recUnitaryFork f

main : ()
main =
    let (p1, f1) = new @ForkExchange () in
    let (p2, f2) = new @ForkExchange () in
    let (p3, f3) = new @ForkExchange () in
    let (p4, f4) = new @ForkExchange () in
    let (p5, f5) = new @ForkExchange () in
    let (p6, f6) = new @ForkExchange () in
    fork @() (\_ : () 1-> fork_ f2 f1);
    fork @() (\_ : () 1-> fork_ f4 f3);
    fork @() (\_ : () 1-> recUnitaryFork f6); --left
    fork @() (\_ : () 1-> recUnitaryFork f5); --right
    fork @() (\_ : () 1-> philosopher 1 p1 p6);
    fork @() (\_ : () 1-> philosopher 2 p2 p3);
    philosopher 3 p4 p5;
    print @String "Done!"

