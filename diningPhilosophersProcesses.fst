type ForkExchange = !();!();Close

philosopher : Int -> ForkExchange -> ForkExchange 1-> ()
philosopher id left right =
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is thinking.");
    let l = send () left in
    let r = send () right in
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
    sendAndClose @() () l;
    sendAndClose @() () r

fork_ : dualof ForkExchange -> dualof ForkExchange 1-> ()
fork_ left right =
    let (i,left) = receive left in
    receiveAndWait @() left;
    let (j,right) = receive right in
    receiveAndWait @() right 

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
    fork @() (\_ : () 1-> philosopher 1 p1 p10);
    fork @() (\_ : () 1-> philosopher 2 p2 p3);
    fork @() (\_ : () 1-> philosopher 3 p4 p5);
    fork @() (\_ : () 1-> philosopher 4 p6 p7);
    fork @() (\_ : () 1-> philosopher 5 p8 p9);
    fork @() (\_ : () 1-> fork_ f1 f2);
    fork @() (\_ : () 1-> fork_ f3 f4);
    fork @() (\_ : () 1-> fork_ f5 f6);
    fork @() (\_ : () 1-> fork_ f7 f8);
    fork_ f9 f10

