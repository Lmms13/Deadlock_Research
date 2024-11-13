type ForkExchange = !();?();Close

philosopher : Int -> ForkExchange 1-> ForkExchange 1-> ()
philosopher id left right =
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is thinking.");
    let left = send () left in
    let right = send () right in
    let (_, left) = receive left in
    let (_, right) = receive right in
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
    close left;
    close right

fork_ : dualof ForkExchange -> dualof ForkExchange 1-> ()
fork_ left right =
    let (_,right) = receive right in
    let right = send () right in
    wait right;
    let (_,left) = receive left in
    let left = send () left in
    wait left

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
    let (p11, f11) = new @ForkExchange () in
    let (p12, f12) = new @ForkExchange () in
    let (p13, f13) = new @ForkExchange () in
    let (p14, f14) = new @ForkExchange () in
    fork @() (\_ : () 1-> fork_ f2 f1);
    fork @() (\_ : () 1-> fork_ f4 f3);
    fork @() (\_ : () 1-> fork_ f5 f6); --here, the forks are passed in the opposite order
    fork @() (\_ : () 1-> fork_ f8 f7);
    fork @() (\_ : () 1-> fork_ f10 f9);
    fork @() (\_ : () 1-> fork_ f12 f11);
    fork @() (\_ : () 1-> fork_ f14 f13);
    fork @() (\_ : () 1-> philosopher 1 p1 p14);
    fork @() (\_ : () 1-> philosopher 2 p3 p2);
    fork @() (\_ : () 1-> philosopher 4 p7 p6);
    fork @() (\_ : () 1-> philosopher 5 p9 p8);
    fork @() (\_ : () 1-> philosopher 6 p11 p10);
    fork @() (\_ : () 1-> philosopher 7 p13 p12);
    philosopher 3 p5 p4;
    print @String "Done!"

