type ForkExchange = !();?();!();Close

philosopher : Int -> ForkExchange 1-> ForkExchange 1-> ()
philosopher id left right =
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is thinking.");
    let l = send () left in
    let r = send () right in
    let (_, l) = receive l in
    let (_, r) = receive r in
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
    sendAndClose @() () l;
    sendAndClose @() () r

fork_ : dualof ForkExchange -> dualof ForkExchange 1-> ()
fork_ left right =
    let (j,right) = receive right in
    let right = send () right in
    receiveAndWait @() right; 
    let (i,left) = receive left in
    let left = send () left in
    receiveAndWait @() left
    --the fork either replies to the philosopher to its left or to its right 
    --first, in this implementation, it replies to the one to its right first,
    --since the philosophers try to grab their left fork first.

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

