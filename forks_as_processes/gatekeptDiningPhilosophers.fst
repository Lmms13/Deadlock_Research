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

fork_ : Int -> dualof ForkExchange -> dualof ForkExchange 1-> ()
fork_ id right left =
    let (_,right) = receive right in
    let right = send () right in
    receiveAndWait @() right; 
    let (_,left) = receive left in
    let left = send () left in
    receiveAndWait @() left

forkLeft : Int -> dualof ForkExchange 1-> ()
forkLeft id left =
    let (_,left) = receive left in
    let left = send () left in
    receiveAndWait @() left

forkRight : Int -> dualof ForkExchange 1-> ()
forkRight id right =
    let (_,right) = receive right in
    let right = send () right in
    receiveAndWait @() right

main : ()
main =
    let (p1, f1) = new @ForkExchange () in
    let (p2, f2) = new @ForkExchange () in
    let (p3, f3) = new @ForkExchange () in
    let (p4, f4) = new @ForkExchange () in
    let (p5, f5) = new @ForkExchange () in
    let (p6, f6) = new @ForkExchange () in
    fork @() (\_ : () 1-> fork_ 1 f1 f2);
    fork @() (\_ : () 1-> fork_ 2 f3 f4);
    fork @() (\_ : () 1-> forkLeft 3 f6);
    fork @() (\_ : () 1-> forkRight 3 f5);
    fork @() (\_ : () 1-> philosopher 1 p1 p6);
    fork @() (\_ : () 1-> philosopher 2 p2 p3);
    philosopher 3 p4 p5

